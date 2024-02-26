import subprocess
import json
from datetime import datetime
import pytz
from concurrent.futures import ThreadPoolExecutor

bucket_name = 'dsc-testbench'
endpoint_url = 'https://s3.deltares.nl/'
threshold_date_str = '2024-02-15'
parallel_processes = 5
threshold_date_obj = datetime.strptime(threshold_date_str, '%Y-%m-%d').replace(tzinfo=pytz.UTC)

print("List all objects in the bucket")
command = f"aws s3api list-object-versions --bucket {bucket_name} --endpoint-url {endpoint_url}"
output = subprocess.check_output(command, shell=True).decode('utf-8', errors='ignore')

print("Convert to json")
output_json = json.loads(output)


def delete_object(version):
    last_modified_str = version['LastModified']
    try:
        last_modified_obj = datetime.strptime(last_modified_str, '%Y-%m-%dT%H:%M:%S.%f%z')
    except ValueError:
        last_modified_obj = datetime.strptime(last_modified_str, '%Y-%m-%dT%H:%M:%S%z')

    if last_modified_obj > threshold_date_obj:
        delete_command = f'aws s3api delete-object --bucket {bucket_name} --key "{version['Key']}" --version-id {version['VersionId']} --endpoint-url {endpoint_url}'
        result = subprocess.run(delete_command, shell=True, capture_output=True)
        if result.returncode == 0:
            print(f"Deleted file: {version['Key']} with timestamp: {last_modified_obj}")
        else:
            print(f"Failed to delete file: {version['Key']} with timestamp: {last_modified_obj}. Error: {result.stderr.decode()}")


with ThreadPoolExecutor(max_workers=parallel_processes) as executor:
    if output_json['Versions']:
        futures = [executor.submit(delete_object, version) for version in output_json['Versions']]
    else:
        print("No versions found in the output.")
