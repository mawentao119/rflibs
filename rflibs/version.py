import re
VERSION = '4.1.1.dev1'


def get_version(naked=False):
    if naked:
        return re.split('(a|b|rc|.dev)', VERSION)[0]
    return VERSION


def get_full_version(program=None, naked=False):
    version = '%s %s (%s %s on %s)' % (program or '',
                                       get_version(naked),
                                       get_interpreter(),
                                       sys.version.split()[0],
                                       sys.platform)
    return version.strip()