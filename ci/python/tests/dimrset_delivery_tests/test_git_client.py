import sys
from unittest.mock import Mock, patch
from unittest.mock import Mock as MockType

from ci_tools.dimrset_delivery.lib.git_client import GitClient


@patch("subprocess.run")
def test_tag_commit_success(mock_run: MockType) -> None:
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    mock_run.return_value.returncode = 0
    # Act
    client.tag_commit("abc123", "v1.0.0")
    # Assert
    assert mock_run.call_count == 2
    args1 = mock_run.call_args_list[0][0][0]
    assert args1[:3] == ["git", "tag", "v1.0.0"]
    args2 = mock_run.call_args_list[1][0][0]
    assert args2[:3] == ["git", "push", "--tags"]


@patch("subprocess.run")
def test_tag_commit_fail_tag(mock_run: MockType) -> None:
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    mock_run.side_effect = [Mock(returncode=1), Mock(returncode=0)]
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.tag_commit("abc123", "v1.0.0")
        # Assert
        mock_exit.assert_called_once()


@patch("subprocess.run")
def test_tag_commit_fail_push(mock_run: MockType) -> None:
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    mock_run.side_effect = [Mock(returncode=0), Mock(returncode=1)]
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.tag_commit("abc123", "v1.0.0")
        # Assert
        mock_exit.assert_called_once()


@patch("subprocess.run")
def test_tag_commit_exception(mock_run: MockType) -> None:
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")

    def raise_exc(*a: object, **kw: object) -> None:
        raise Exception("fail")

    mock_run.side_effect = raise_exc
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.tag_commit("abc123", "v1.0.0")
        # Assert
        mock_exit.assert_called_once()


@patch("subprocess.run")
def test_test_connection_success(mock_run: MockType) -> None:
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    mock_run.return_value = Mock(returncode=0)
    # Act
    client.test_connection(dry_run=False)
    # Assert
    mock_run.assert_called_once()


@patch("subprocess.run")
def test_test_connection_fail(mock_run: MockType) -> None:
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    mock_run.return_value = Mock(returncode=1)
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.test_connection(dry_run=False)
        # Assert
        mock_exit.assert_called_once()


@patch("subprocess.run")
def test_test_connection_exception(mock_run: MockType) -> None:
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")

    def raise_exc(*a: object, **kw: object) -> None:
        raise Exception("fail")

    mock_run.side_effect = raise_exc
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.test_connection(dry_run=False)
        # Assert
        mock_exit.assert_called_once()


def test_test_connection_dry_run() -> None:
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    # Act
    client.test_connection(dry_run=True)
    # Assert
    # No exception should be raised
    assert True
