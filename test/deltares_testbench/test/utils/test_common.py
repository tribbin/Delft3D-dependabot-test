from unittest.mock import Mock, patch, call
from src.utils.logging.log_level import LogLevel
from src.utils.common import log_header, log_separator, log_table

class TestCommon:
    @patch('src.utils.logging.console_logger.ConsoleLogger')
    def test_log_header(self, mock_logger):
        logger_instance = Mock()

        # Arrange
        mock_logger.return_value = logger_instance

        # Act
        log_header("Sample Header", logger_instance, LogLevel.INFO, 10, "*")

        # Assert
        expected_calls = [
            call('**********', LogLevel.INFO),
            call('Sample Header', LogLevel.INFO),
            call('**********', LogLevel.INFO),
        ]

        logger_instance.log.assert_has_calls(expected_calls)

    @patch('src.utils.logging.console_logger.ConsoleLogger')
    def test_log_separator(self, mock_logger):
        # Arrange
        logger_instance = Mock()
        mock_logger.return_value = logger_instance

        # Act
        log_separator(logger_instance, LogLevel.INFO, 3, 'v')

        # Assert
        logger_instance.log.assert_called_with('vvv', LogLevel.INFO)

    @patch('src.utils.logging.console_logger.ConsoleLogger')
    def test_log_table(self, mock_logger):
        # Arrange
        logger_instance = Mock()
        mock_logger.return_value = logger_instance

        sample_table = {
            "Header 1": ["2", "5"],
            "Header 2": ["test string", "test 2"],
            "Header 3": ["3.9", "2.6"],
        }

        # Act
        log_table(sample_table, logger_instance, LogLevel.INFO, "-")

        # Assert
        expected_calls = [
            call('-------------------------------', LogLevel.INFO),
            call('|Header 1|Header 2   |Header 3|', LogLevel.INFO),
            call('-------------------------------', LogLevel.INFO),
            call('|2       |test string|3.9     |', LogLevel.INFO),
            call('|5       |test 2     |2.6     |', LogLevel.INFO),
            call('-------------------------------', LogLevel.INFO),
        ]

        logger_instance.log.assert_has_calls(expected_calls)

    @patch('src.utils.logging.console_logger.ConsoleLogger')
    def test_log_table_real_example(self, mock_logger):
        # Arrange
        logger_instance = Mock()
        mock_logger.return_value = logger_instance

        sample_table = {
            "Test case name": ["e02_f029_c501_junction-advection-acceleration-equidistant_WAQ"],
            "Runtime": ["1.177e+00"],
            "Ratio": ["1.177e+00"],
            "Result": ["OK"],
            "MaxAbsDiff": ["8.881784197001252e-16"],
            "MaxRelDiff": ["1.5340265946406362e-08"],
            "File name": ["DFM_DELWAQ_Water_Quality/deltashell_map.nc"],
            "Parameter name": ["mesh1d_"],
            "Information": ["at coordinates()"],
        }

        # Act
        log_table(sample_table, logger_instance, LogLevel.INFO, "-")

        # Assert
        expected_calls = [
            call('------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------', LogLevel.INFO),
            call('|Test case name                                               |Runtime  |Ratio    |Result|MaxAbsDiff           |MaxRelDiff            |File name                                 |Parameter name|Information     |', LogLevel.INFO),
            call('------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------', LogLevel.INFO),
            call('|e02_f029_c501_junction-advection-acceleration-equidistant_WAQ|1.177e+00|1.177e+00|OK    |8.881784197001252e-16|1.5340265946406362e-08|DFM_DELWAQ_Water_Quality/deltashell_map.nc|mesh1d_       |at coordinates()|', LogLevel.INFO),
            call('------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------', LogLevel.INFO),
        ]

        logger_instance.log.assert_has_calls(expected_calls)