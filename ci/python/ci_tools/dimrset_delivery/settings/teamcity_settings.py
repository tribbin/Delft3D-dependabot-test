from enum import Enum


class KernelData():
    """Class to hold the various variations of names for a kernel."""

    def __init__(self, name_for_extracting_revision: str, name_for_email: str) -> None:
        """
        Create a new instance of KernelData.

        Args:
            name_for_extracting_revision (str): The name of the kernel used to extract the revision number from
            the build dependencies.
            name_for_email (str): The name to represent the kernel in the email.
        """
        self.name_for_extracting_revision = name_for_extracting_revision
        self.name_for_email = name_for_email


# All kernels for which the versions are set in the configuration parameters of 2.Dimr_collector_release
KERNELS = [
    KernelData(name_for_extracting_revision="DIMRset_ver", name_for_email="DIMRset"),
    KernelData(name_for_extracting_revision="build.vcs.number", name_for_email="OSS"),
]


class TeamcityIds(Enum):
    """Enumeration of TeamCity build configuration IDs used in DIMR automation."""

    DIMR_PUBLISH = "Delft3D_DIMRbak"
    DELFT3D_LINUX_COLLECT_BUILD_TYPE_ID = "Delft3D_LinuxCollect"
    DELFT3D_WINDOWS_COLLECT_BUILD_TYPE_ID = "Delft3D_WindowsCollect"
    DIMR_TO_NGHS_BUILD_TYPE_ID = "DIMR_To_NGHS"
    DIMR_TESTBENCH_RELEASE_TESTS_LINUX = "Dimr_DimrCollectors_DIMRsetAggregatedReleaseResultsLinux"
    DIMR_TESTBENCH_RELEASE_TESTS_WINDOWS = "Dimr_DimrCollectors_DIMRsetAggregatedReleaseResultsWindows"
    STATUS_OF_DAILY = "Dimr_DimrTestbenchRelease_StatusOfDailyTestbench"


# Path to Windows version artifact on TeamCity
PATH_TO_WINDOWS_VERSION_ARTIFACT = "version/dimrset_version_x64.txt"

# Path to Linux version artifact on TeamCity
PATH_TO_LINUX_VERSION_ARTIFACT = "version/dimrset_version_lnx64.txt"

# Path to release test results artifact on TeamCity
PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT = "teamcity_test_results.txt"

# Path to summary of DIMR collector release signed artifact on TeamCity
PATH_TO_DIMR_COLLECTOR_RELEASE_SIGNED_ARTIFACT = "signed/summary.txt"

# Name of the DIMR set release signed Linux artifact
NAME_OF_DIMR_RELEASE_SIGNED_LINUX_ARTIFACT = "dimrset_lnx64"

# Name of the DIMR set release signed Windows artifact
NAME_OF_DIMR_RELEASE_SIGNED_WINDOWS_ARTIFACT = "dimrset_x64"
