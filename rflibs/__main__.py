"""Various generic utility functions and classes.

Utilities are mainly for internal usage, but external libraries and tools
may find some of them useful. Utilities are generally stable, but absolute
backwards compatibility between major versions is not guaranteed.

All utilities are exposed via the :mod:`rflibs.utils` package, and should be
used either like::

    from rflibs import OSLIB

    OSLIB.should_exist("test1.py")

or::

    from rflibs.utils import Matcher

    assert Matcher('H?llo').match('Hillo')
"""
VERSION = "0.0.1"

