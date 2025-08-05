"""Shared fixtures for DIMRset delivery tests."""

import pytest
from unittest.mock import Mock


@pytest.fixture
def mock_dimr_context():
    """Create a mock DimrAutomationContext for testing."""
    context = Mock()
    context.build_id = "12345"
    context.dry_run = False
    
    # Mock TeamCity client
    context.teamcity = Mock()
    context.teamcity._TeamCity__rest_uri = "https://test.com/app/rest/"
    context.teamcity._TeamCity__default_headers = {}
    context.teamcity._TeamCity__auth = ("test_user", "test_pass")
    context.teamcity.get_full_build_info_for_build_id.return_value = None
    context.teamcity.get_filtered_dependent_build_ids.return_value = []
    
    return context


@pytest.fixture
def sample_build_info():
    """Sample build info for testing."""
    return {
        "number": "456",
        "status": "SUCCESS",
        "buildType": {
            "name": "Test Configuration",
            "projectName": "Test Project"
        },
        "testOccurrences": {
            "count": "10",
            "passed": "8",
            "failed": "1",
            "ignored": "1",
            "muted": "0"
        }
    }


@pytest.fixture
def sample_configuration_test_results():
    """Sample configuration test results for testing."""
    from src.teamcity_retrieve_engine_test_status_dpc import ConfigurationTestResult
    
    return [
        ConfigurationTestResult("Config1", "123", 10, 2, 1, 1, "SUCCESS"),
        ConfigurationTestResult("Config2", "124", 8, 1, 0, 0, "SUCCESS"),
        ConfigurationTestResult("Config3", "125", 5, 0, 1, 0, "SUCCESS"),
    ]
