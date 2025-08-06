#!/usr/bin/env python3
"""Assert preconditions are met before the DIMR release process is run."""

import logging
import os

from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    create_context_from_args,
    parse_common_arguments,
)
from ci_tools.dimrset_delivery.settings.general_settings import DRY_RUN_PREFIX, LINUX_ADDRESS, NETWORK_BASE_PATH

logger = logging.getLogger(__name__)


def _check_api_connections(context: DimrAutomationContext) -> None:
    """Check API connections for TeamCity and Atlassian.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.

    Raises
    ------
    ValueError
        If required clients are not initialized.
    AssertionError
        If any API connection fails.
    """
    logger.info("Checking API connections...")

    if context.teamcity is None:
        raise ValueError("TeamCity client is required but not initialized")

    logger.debug("Testing TeamCity API connection...")
    if not context.teamcity.test_api_connection(context.dry_run):
        raise AssertionError("Failed to connect to the TeamCity REST API.")

    logger.info("TeamCity API connection successful")

    if context.atlassian is None:
        raise ValueError("Atlassian client is required but not initialized")

    logger.debug("Testing Atlassian API connection...")
    if not context.atlassian.test_api_connection(context.dry_run):
        raise AssertionError("Failed to connect to the Atlassian Confluence REST API.")

    logger.info("Atlassian API connection successful")


def _check_network_access(dry_run: bool) -> None:
    """Check read/write access to the network drive.

    Parameters
    ----------
    dry_run : bool
        Whether to run in dry-run mode without making actual checks.

    Raises
    ------
    AssertionError
        If network access check fails.
    """
    print("Checking read/write access to the network drive...")
    if dry_run:
        print(f"{DRY_RUN_PREFIX} Checking read/write access to {NETWORK_BASE_PATH}")
        print(f"Successfully checked for read and write access to {NETWORK_BASE_PATH}.")
    else:
        try:
            if not os.path.exists(NETWORK_BASE_PATH):
                raise AssertionError(f"Network path does not exist: {NETWORK_BASE_PATH}")

            if not (os.access(NETWORK_BASE_PATH, os.W_OK) and os.access(NETWORK_BASE_PATH, os.R_OK)):
                raise AssertionError(f"Insufficient permissions for {NETWORK_BASE_PATH}")

            print(f"Successfully checked for read and write access to {NETWORK_BASE_PATH}.")
        except OSError as e:
            raise AssertionError(f"Could not access {NETWORK_BASE_PATH}: {e}") from e


def _check_ssh_connection(context: DimrAutomationContext) -> None:
    """Check SSH connection to Linux server.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.

    Raises
    ------
    AssertionError
        If SSH connection fails.
    """
    print("Checking if ssh connection to Linux can be made...")
    if context.ssh_client is None:
        raise ValueError("SSH client is required but not initialized")

    try:
        context.ssh_client.test_connection(LINUX_ADDRESS, context.dry_run)
    except Exception as e:
        raise AssertionError(f"Could not establish ssh connection to {LINUX_ADDRESS}: {e}") from e


def _check_git_connection(context: DimrAutomationContext) -> None:
    """Check Git connection.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.

    Raises
    ------
    AssertionError
        If Git connection fails.
    """
    print("Checking if git connection can be made...")
    if context.git_client is None:
        raise ValueError("Git client is required but not initialized")

    try:
        context.git_client.test_connection(context.dry_run)
    except Exception as e:
        raise AssertionError(f"Could not establish git connection: {e}") from e


def assert_preconditions(context: DimrAutomationContext) -> None:
    """Assert that all preconditions are met before the script is fully run.

    This function performs comprehensive checks to ensure all required services,
    connections, and permissions are available before executing the main automation.

    The checks include:
    - API connectivity to TeamCity and Atlassian services
    - Network drive access permissions
    - SSH connectivity to Linux servers
    - Git repository access

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.

    Raises
    ------
    ValueError
        If any required client is not initialized.
    AssertionError
        If any precondition check fails.
    """
    context.print_status("Asserting preconditions...")
    logger.info("Starting comprehensive preconditions check...")

    try:
        # Check all required API connections
        _check_api_connections(context)

        # Check network drive access
        _check_network_access(context.dry_run)

        # Check SSH connection
        _check_ssh_connection(context)

        # Check Git connection
        _check_git_connection(context)

        logger.info("All preconditions successfully verified")
        print("Successfully asserted all preconditions.")
        print("Preconditions check completed successfully!")

    except (ValueError, AssertionError) as e:
        logger.error("Preconditions check failed: %s", str(e))
        raise
    except Exception as e:
        logger.exception("Unexpected error during preconditions check")
        raise AssertionError(f"Unexpected error during preconditions check: {e}") from e


if __name__ == "__main__":
    import sys

    # Configure basic logging
    logging.basicConfig(
        level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s", datefmt="%Y-%m-%d %H:%M:%S"
    )

    try:
        args = parse_common_arguments()
        context = create_context_from_args(args)

        print("Starting preconditions check...")
        logger.info("Preconditions check initiated from command line")

        assert_preconditions(context)

        print("Finished successfully!")
        logger.info("Preconditions check completed successfully")
        sys.exit(0)

    except KeyboardInterrupt:
        print("\nPreconditions check interrupted by user")
        logger.warning("Preconditions check interrupted by user")
        sys.exit(130)  # Standard exit code for keyboard interrupt

    except (ValueError, AssertionError) as e:
        print(f"Preconditions check failed: {e}")
        logger.error("Preconditions check failed: %s", str(e))
        sys.exit(1)

    except Exception as e:
        print(f"Unexpected error: {e}")
        logger.exception("Unexpected error during preconditions check")
        sys.exit(2)
