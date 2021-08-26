import logging


def write(msg, level='INFO', html=False):
    """Writes the message to the log file using the given level.

    Valid log levels are ``TRACE``, ``DEBUG``, ``INFO`` (default), ``WARN``, and
    ``ERROR``. Additionally it is possible to use ``HTML`` pseudo log level that
    logs the message as HTML using the ``INFO`` level.

    Instead of using this method, it is generally better to use the level
    specific methods such as ``info`` and ``debug`` that have separate
    ``html`` argument to control the message format.
    """
    logger = logging.getLogger("RobotFramework")
    level = {'TRACE': logging.DEBUG // 2,
             'DEBUG': logging.DEBUG,
             'INFO': logging.INFO,
             'HTML': logging.INFO,
             'WARN': logging.WARN,
             'ERROR': logging.ERROR}[level]
    logger.log(level, msg)


def trace(msg, html=False):
    """Writes the message to the log file using the ``TRACE`` level."""
    write(msg, 'TRACE', html)


def debug(msg, html=False):
    """Writes the message to the log file using the ``DEBUG`` level."""
    write(msg, 'DEBUG', html)


def info(msg, html=False, also_console=False):
    """Writes the message to the log file using the ``INFO`` level.

    If ``also_console`` argument is set to ``True``, the message is
    written both to the log file and to the console.
    """
    write(msg, 'INFO', html)
    if also_console:
        console(msg)


def warn(msg, html=False):
    """Writes the message to the log file using the ``WARN`` level."""
    write(msg, 'WARN', html)


def error(msg, html=False):
    """Writes the message to the log file using the ``ERROR`` level.
    """
    write(msg, 'ERROR', html)


def console(msg, newline=True, stream='stdout'):
    """Writes the message to the console.

    If the ``newline`` argument is ``True``, a newline character is
    automatically added to the message.

    By default the message is written to the standard output stream.
    Using the standard error stream is possibly by giving the ``stream``
    argument value ``'stderr'``.
    """
    logging.console(msg, newline, stream)
