#  Copyright 2008-2015 Nokia Networks
#  Copyright 2016-     Robot Framework Foundation
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

from __future__ import absolute_import

import difflib
import re

from .. import logger

from ..utils import (is_unicode, get_error_message, Matcher,
                         is_integer, is_list_like, is_string, is_truthy,
                         IRONPYTHON, JYTHON, normalize,
                         plural_or_not as s, RERAISED_EXCEPTIONS,
                         roundup, seq2str, split_from_equals,unic)
from ..utils.asserts import assert_equal, assert_not_equal

if JYTHON:
    from java.lang import String, Number

class _BuiltInBase(object):

    def _matches(self, string, pattern, caseless=False):
        # Must use this instead of fnmatch when string may contain newlines.
        matcher = Matcher(pattern, caseless=caseless, spaceless=False)
        return matcher.match(string)

    def _is_true(self, condition):
        if is_string(condition):
            condition = self.evaluate(condition)
        return bool(condition)

    def _log_types(self, *args):
        self._log_types_at_level('DEBUG', *args)

    def _log_types_at_level(self, level, *args):
        msg = ["Argument types are:"] + [self._get_type(a) for a in args]
        logger.write('\n'.join(msg), level)

    def _get_type(self, arg):
        # In IronPython type(u'x') is str. We want to report unicode anyway.
        if is_unicode(arg):
            return "<type 'unicode'>"
        return str(type(arg))

class _Converter(_BuiltInBase):

    def convert_to_integer(self, item, base=None):
        """Converts the given item to an integer number.

        If the given item is a string, it is by default expected to be an
        integer in base 10. There are two ways to convert from other bases:

        - Give base explicitly to the keyword as ``base`` argument.

        - Prefix the given string with the base so that ``0b`` means binary
          (base 2), ``0o`` means octal (base 8), and ``0x`` means hex (base 16).
          The prefix is considered only when ``base`` argument is not given and
          may itself be prefixed with a plus or minus sign.

        The syntax is case-insensitive and possible spaces are ignored.

        Examples:
        | ${result} = | Convert To Integer | 100    |    | # Result is 100   |
        | ${result} = | Convert To Integer | FF AA  | 16 | # Result is 65450 |
        | ${result} = | Convert To Integer | 100    | 8  | # Result is 64    |
        | ${result} = | Convert To Integer | -100   | 2  | # Result is -4    |
        | ${result} = | Convert To Integer | 0b100  |    | # Result is 4     |
        | ${result} = | Convert To Integer | -0x100 |    | # Result is -256  |

        See also `Convert To Number`, `Convert To Binary`, `Convert To Octal`,
        `Convert To Hex`, and `Convert To Bytes`.
        """
        self._log_types(item)
        return self._convert_to_integer(item, base)

    def _convert_to_integer(self, orig, base=None):
        try:
            item = self._handle_java_numbers(orig)
            item, base = self._get_base(item, base)
            if base:
                return int(item, self._convert_to_integer(base))
            return int(item)
        except:
            raise RuntimeError("'%s' cannot be converted to an integer: %s"
                               % (orig, get_error_message()))

    def _handle_java_numbers(self, item):
        if not JYTHON:
            return item
        if isinstance(item, String):
            return unic(item)
        if isinstance(item, Number):
            return item.doubleValue()
        return item

    def _get_base(self, item, base):
        if not is_string(item):
            return item, base
        item = normalize(item)
        if item.startswith(('-', '+')):
            sign = item[0]
            item = item[1:]
        else:
            sign = ''
        bases = {'0b': 2, '0o': 8, '0x': 16}
        if base or not item.startswith(tuple(bases)):
            return sign+item, base
        return sign+item[2:], bases[item[:2]]

    def convert_to_binary(self, item, base=None, prefix=None, length=None):
        """Converts the given item to a binary string.

        The ``item``, with an optional ``base``, is first converted to an
        integer using `Convert To Integer` internally. After that it
        is converted to a binary number (base 2) represented as a
        string such as ``1011``.

        The returned value can contain an optional ``prefix`` and can be
        required to be of minimum ``length`` (excluding the prefix and a
        possible minus sign). If the value is initially shorter than
        the required length, it is padded with zeros.

        Examples:
        | ${result} = | Convert To Binary | 10 |         |           | # Result is 1010   |
        | ${result} = | Convert To Binary | F  | base=16 | prefix=0b | # Result is 0b1111 |
        | ${result} = | Convert To Binary | -2 | prefix=B | length=4 | # Result is -B0010 |

        See also `Convert To Integer`, `Convert To Octal` and `Convert To Hex`.
        """
        return self._convert_to_bin_oct_hex(item, base, prefix, length, 'b')

    def convert_to_octal(self, item, base=None, prefix=None, length=None):
        """Converts the given item to an octal string.

        The ``item``, with an optional ``base``, is first converted to an
        integer using `Convert To Integer` internally. After that it
        is converted to an octal number (base 8) represented as a
        string such as ``775``.

        The returned value can contain an optional ``prefix`` and can be
        required to be of minimum ``length`` (excluding the prefix and a
        possible minus sign). If the value is initially shorter than
        the required length, it is padded with zeros.

        Examples:
        | ${result} = | Convert To Octal | 10 |            |          | # Result is 12      |
        | ${result} = | Convert To Octal | -F | base=16    | prefix=0 | # Result is -017    |
        | ${result} = | Convert To Octal | 16 | prefix=oct | length=4 | # Result is oct0020 |

        See also `Convert To Integer`, `Convert To Binary` and `Convert To Hex`.
        """
        return self._convert_to_bin_oct_hex(item, base, prefix, length, 'o')

    def convert_to_hex(self, item, base=None, prefix=None, length=None,
                       lowercase=False):
        """Converts the given item to a hexadecimal string.

        The ``item``, with an optional ``base``, is first converted to an
        integer using `Convert To Integer` internally. After that it
        is converted to a hexadecimal number (base 16) represented as
        a string such as ``FF0A``.

        The returned value can contain an optional ``prefix`` and can be
        required to be of minimum ``length`` (excluding the prefix and a
        possible minus sign). If the value is initially shorter than
        the required length, it is padded with zeros.

        By default the value is returned as an upper case string, but the
        ``lowercase`` argument a true value (see `Boolean arguments`) turns
        the value (but not the given prefix) to lower case.

        Examples:
        | ${result} = | Convert To Hex | 255 |           |              | # Result is FF    |
        | ${result} = | Convert To Hex | -10 | prefix=0x | length=2     | # Result is -0x0A |
        | ${result} = | Convert To Hex | 255 | prefix=X | lowercase=yes | # Result is Xff   |

        See also `Convert To Integer`, `Convert To Binary` and `Convert To Octal`.
        """
        spec = 'x' if lowercase else 'X'
        return self._convert_to_bin_oct_hex(item, base, prefix, length, spec)

    def _convert_to_bin_oct_hex(self, item, base, prefix, length, format_spec):
        self._log_types(item)
        ret = format(self._convert_to_integer(item, base), format_spec)
        prefix = prefix or ''
        if ret[0] == '-':
            prefix = '-' + prefix
            ret = ret[1:]
        if length:
            ret = ret.rjust(self._convert_to_integer(length), '0')
        return prefix + ret

    def convert_to_number(self, item, precision=None):
        """Converts the given item to a floating point number.

        If the optional ``precision`` is positive or zero, the returned number
        is rounded to that number of decimal digits. Negative precision means
        that the number is rounded to the closest multiple of 10 to the power
        of the absolute precision. If a number is equally close to a certain
        precision, it is always rounded away from zero.

        Examples:
        | ${result} = | Convert To Number | 42.512 |    | # Result is 42.512 |
        | ${result} = | Convert To Number | 42.512 | 1  | # Result is 42.5   |
        | ${result} = | Convert To Number | 42.512 | 0  | # Result is 43.0   |
        | ${result} = | Convert To Number | 42.512 | -1 | # Result is 40.0   |

        Notice that machines generally cannot store floating point numbers
        accurately. This may cause surprises with these numbers in general
        and also when they are rounded. For more information see, for example,
        these resources:

        - http://docs.python.org/tutorial/floatingpoint.html
        - http://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition

        If you want to avoid possible problems with floating point numbers,
        you can implement custom keywords using Python's
        [http://docs.python.org/library/decimal.html|decimal] or
        [http://docs.python.org/library/fractions.html|fractions] modules.

        If you need an integer number, use `Convert To Integer` instead.
        """
        self._log_types(item)
        return self._convert_to_number(item, precision)

    def _convert_to_number(self, item, precision=None):
        number = self._convert_to_number_without_precision(item)
        if precision is not None:
            number = roundup(number, self._convert_to_integer(precision),
                             return_type=float)
        return number

    def _convert_to_number_without_precision(self, item):
        try:
            if JYTHON:
                item = self._handle_java_numbers(item)
            return float(item)
        except:
            error = get_error_message()
            try:
                return float(self._convert_to_integer(item))
            except RuntimeError:
                raise RuntimeError("'%s' cannot be converted to a floating "
                                   "point number: %s" % (item, error))

    def convert_to_string(self, item):
        """Converts the given item to a Unicode string.

        Strings are also [http://www.macchiato.com/unicode/nfc-faq|
        NFC normalized].

        Use `Encode String To Bytes` and `Decode Bytes To String` keywords
        in ``String`` library if you need to convert between Unicode and byte
        strings using different encodings. Use `Convert To Bytes` if you just
        want to create byte strings.
        """
        self._log_types(item)
        return self._convert_to_string(item)

    def _convert_to_string(self, item):
        return unic(item)

    def convert_to_boolean(self, item):
        """Converts the given item to Boolean true or false.

        Handles strings ``True`` and ``False`` (case-insensitive) as expected,
        otherwise returns item's
        [http://docs.python.org/library/stdtypes.html#truth|truth value]
        using Python's ``bool()`` method.
        """
        self._log_types(item)
        if is_string(item):
            if item.upper() == 'TRUE':
                return True
            if item.upper() == 'FALSE':
                return False
        return bool(item)

    def convert_to_bytes(self, input, input_type='text'):
        u"""Converts the given ``input`` to bytes according to the ``input_type``.

        Valid input types are listed below:

        - ``text:`` Converts text to bytes character by character. All
          characters with ordinal below 256 can be used and are converted to
          bytes with same values. Many characters are easiest to represent
          using escapes like ``\\x00`` or ``\\xff``. Supports both Unicode
          strings and bytes.

        - ``int:`` Converts integers separated by spaces to bytes. Similarly as
          with `Convert To Integer`, it is possible to use binary, octal, or
          hex values by prefixing the values with ``0b``, ``0o``, or ``0x``,
          respectively.

        - ``hex:`` Converts hexadecimal values to bytes. Single byte is always
          two characters long (e.g. ``01`` or ``FF``). Spaces are ignored and
          can be used freely as a visual separator.

        - ``bin:`` Converts binary values to bytes. Single byte is always eight
          characters long (e.g. ``00001010``). Spaces are ignored and can be
          used freely as a visual separator.

        In addition to giving the input as a string, it is possible to use
        lists or other iterables containing individual characters or numbers.
        In that case numbers do not need to be padded to certain length and
        they cannot contain extra spaces.

        Examples (last column shows returned bytes):
        | ${bytes} = | Convert To Bytes | hyv\xe4    |     | # hyv\\xe4        |
        | ${bytes} = | Convert To Bytes | \\xff\\x07 |     | # \\xff\\x07      |
        | ${bytes} = | Convert To Bytes | 82 70      | int | # RF              |
        | ${bytes} = | Convert To Bytes | 0b10 0x10  | int | # \\x02\\x10      |
        | ${bytes} = | Convert To Bytes | ff 00 07   | hex | # \\xff\\x00\\x07 |
        | ${bytes} = | Convert To Bytes | 5246212121 | hex | # RF!!!           |
        | ${bytes} = | Convert To Bytes | 0000 1000  | bin | # \\x08           |
        | ${input} = | Create List      | 1          | 2   | 12                |
        | ${bytes} = | Convert To Bytes | ${input}   | int | # \\x01\\x02\\x0c |
        | ${bytes} = | Convert To Bytes | ${input}   | hex | # \\x01\\x02\\x12 |

        Use `Encode String To Bytes` in ``String`` library if you need to
        convert text to bytes using a certain encoding.
        """
        try:
            try:
                ordinals = getattr(self, '_get_ordinals_from_%s' % input_type)
            except AttributeError:
                raise RuntimeError("Invalid input type '%s'." % input_type)
            return bytes(bytearray(o for o in ordinals(input)))
        except:
            raise RuntimeError("Creating bytes failed: %s" % get_error_message())

    def _get_ordinals_from_text(self, input):
        # https://github.com/IronLanguages/main/issues/1237
        if IRONPYTHON and isinstance(input, bytearray):
            input = bytes(input)
        for char in input:
            ordinal = char if is_integer(char) else ord(char)
            yield self._test_ordinal(ordinal, char, 'Character')

    def _test_ordinal(self, ordinal, original, type):
        if 0 <= ordinal <= 255:
            return ordinal
        raise RuntimeError("%s '%s' cannot be represented as a byte."
                           % (type, original))

    def _get_ordinals_from_int(self, input):
        if is_string(input):
            input = input.split()
        elif is_integer(input):
            input = [input]
        for integer in input:
            ordinal = self._convert_to_integer(integer)
            yield self._test_ordinal(ordinal, integer, 'Integer')

    def _get_ordinals_from_hex(self, input):
        for token in self._input_to_tokens(input, length=2):
            ordinal = self._convert_to_integer(token, base=16)
            yield self._test_ordinal(ordinal, token, 'Hex value')

    def _get_ordinals_from_bin(self, input):
        for token in self._input_to_tokens(input, length=8):
            ordinal = self._convert_to_integer(token, base=2)
            yield self._test_ordinal(ordinal, token, 'Binary value')

    def _input_to_tokens(self, input, length):
        if not is_string(input):
            return input
        input = ''.join(input.split())
        if len(input) % length != 0:
            raise RuntimeError('Expected input to be multiple of %d.' % length)
        return (input[i:i+length] for i in range(0, len(input), length))

    def create_list(self, *items):
        """Returns a list containing given items.

        The returned list can be assigned both to ``${scalar}`` and ``@{list}``
        variables.

        Examples:
        | @{list} =   | Create List | a    | b    | c    |
        | ${scalar} = | Create List | a    | b    | c    |
        | ${ints} =   | Create List | ${1} | ${2} | ${3} |
        """
        return list(items)


class _Verify(_BuiltInBase):

    def _set_and_remove_tags(self, tags):
        set_tags = [tag for tag in tags if not tag.startswith('-')]
        remove_tags = [tag[1:] for tag in tags if tag.startswith('-')]
        if remove_tags:
            self.remove_tags(*remove_tags)
        if set_tags:
            self.set_tags(*set_tags)

    def fail(self, msg=None, *tags):
        """Fails the test with the given message and optionally alters its tags.

        The error message is specified using the ``msg`` argument.
        It is possible to use HTML in the given error message, similarly
        as with any other keyword accepting an error message, by prefixing
        the error with ``*HTML*``.

        It is possible to modify tags of the current test case by passing tags
        after the message. Tags starting with a hyphen (e.g. ``-regression``)
        are removed and others added. Tags are modified using `Set Tags` and
        `Remove Tags` internally, and the semantics setting and removing them
        are the same as with these keywords.

        Examples:
        | Fail | Test not ready   |             | | # Fails with the given message.    |
        | Fail | *HTML*<b>Test not ready</b> | | | # Fails using HTML in the message. |
        | Fail | Test not ready   | not-ready   | | # Fails and adds 'not-ready' tag.  |
        | Fail | OS not supported | -regression | | # Removes tag 'regression'.        |
        | Fail | My message       | tag    | -t*  | # Removes all tags starting with 't' except the newly added 'tag'. |

        See `Fatal Error` if you need to stop the whole test execution.
        """
        self._set_and_remove_tags(tags)
        raise AssertionError(msg) if msg else AssertionError()

    def fatal_error(self, msg=None):
        """Stops the whole test execution.

        The test or suite where this keyword is used fails with the provided
        message, and subsequent tests fail with a canned message.
        Possible teardowns will nevertheless be executed.

        See `Fail` if you only want to stop one test case unconditionally.
        """
        error = AssertionError(msg) if msg else AssertionError()
        error.ROBOT_EXIT_ON_FAILURE = True
        raise error

    def should_not_be_true(self, condition, msg=None):
        """Fails if the given condition is true.

        See `Should Be True` for details about how ``condition`` is evaluated
        and how ``msg`` can be used to override the default error message.
        """
        if self._is_true(condition):
            raise AssertionError(msg or "'%s' should not be true." % condition)

    def should_be_true(self, condition, msg=None):
        """Fails if the given condition is not true.

        If ``condition`` is a string (e.g. ``${rc} < 10``), it is evaluated as
        a Python expression as explained in `Evaluating expressions` and the
        keyword status is decided based on the result. If a non-string item is
        given, the status is got directly from its
        [http://docs.python.org/library/stdtypes.html#truth|truth value].

        The default error message (``<condition> should be true``) is not very
        informative, but it can be overridden with the ``msg`` argument.

        Examples:
        | Should Be True | ${rc} < 10            |
        | Should Be True | '${status}' == 'PASS' | # Strings must be quoted |
        | Should Be True | ${number}   | # Passes if ${number} is not zero |
        | Should Be True | ${list}     | # Passes if ${list} is not empty  |

        Variables used like ``${variable}``, as in the examples above, are
        replaced in the expression before evaluation. Variables are also
        available in the evaluation namespace, and can be accessed using
        special ``$variable`` syntax as explained in the `Evaluating
        expressions` section.

        Examples:
        | Should Be True | $rc < 10          |
        | Should Be True | $status == 'PASS' | # Expected string must be quoted |

        `Should Be True` automatically imports Python's
        [http://docs.python.org/library/os.html|os] and
        [http://docs.python.org/library/sys.html|sys] modules that contain
        several useful attributes:

        | Should Be True | os.linesep == '\\n'             | # Unixy   |
        | Should Be True | os.linesep == '\\r\\n'          | # Windows |
        | Should Be True | sys.platform == 'darwin'        | # OS X    |
        | Should Be True | sys.platform.startswith('java') | # Jython  |
        """
        if not self._is_true(condition):
            raise AssertionError(msg or "'%s' should be true." % condition)

    def should_be_equal(self, first, second, msg=None, values=True,
                        ignore_case=False, formatter='str', strip_spaces=False,
                        collapse_spaces=False):
        """Fails if the given objects are unequal.

        Optional ``msg``, ``values`` and ``formatter`` arguments specify how
        to construct the error message if this keyword fails:

        - If ``msg`` is not given, the error message is ``<first> != <second>``.
        - If ``msg`` is given and ``values`` gets a true value (default),
          the error message is ``<msg>: <first> != <second>``.
        - If ``msg`` is given and ``values`` gets a false value (see
          `Boolean arguments`), the error message is simply ``<msg>``.
        - ``formatter`` controls how to format the values. Possible values are
          ``str`` (default), ``repr`` and ``ascii``, and they work similarly
          as Python built-in functions with same names. See `String
          representations` for more details.

        If ``ignore_case`` is given a true value (see `Boolean arguments`) and
        both arguments are strings, comparison is done case-insensitively.
        If both arguments are multiline strings, this keyword uses
        `multiline string comparison`.

        If ``strip_spaces`` is given a true value (see `Boolean arguments`)
        and both arguments are strings, the comparison is done without leading
        and trailing spaces. If ``strip_spaces`` is given a string value
        ``LEADING`` or ``TRAILING`` (case-insensitive), the comparison is done
        without leading or trailing spaces, respectively.

        If ``collapse_spaces`` is given a true value (see `Boolean arguments`) and both
        arguments are strings, the comparison is done with all white spaces replaced by
        a single space character.

        Examples:
        | Should Be Equal | ${x} | expected |
        | Should Be Equal | ${x} | expected | Custom error message |
        | Should Be Equal | ${x} | expected | Custom message | values=False |
        | Should Be Equal | ${x} | expected | ignore_case=True | formatter=repr |

        ``strip_spaces`` is new in Robot Framework 4.0 and
        ``collapse_spaces`` is new in Robot Framework 4.1.
        """
        self._log_types_at_info_if_different(first, second)
        if is_string(first) and is_string(second):
            if ignore_case:
                first = first.lower()
                second = second.lower()
            if strip_spaces:
                first = self._strip_spaces(first, strip_spaces)
                second = self._strip_spaces(second, strip_spaces)
            if collapse_spaces:
                first = self._collapse_spaces(first)
                second = self._collapse_spaces(second)
        self._should_be_equal(first, second, msg, values, formatter)

    def _should_be_equal(self, first, second, msg, values, formatter='str'):
        include_values = self._include_values(values)
        formatter = self._get_formatter(formatter)
        if first == second:
            return
        if include_values and is_string(first) and is_string(second):
            self._raise_multi_diff(first, second, formatter)
        assert_equal(first, second, msg, include_values, formatter)

    def _log_types_at_info_if_different(self, first, second):
        level = 'DEBUG' if type(first) == type(second) else 'INFO'
        self._log_types_at_level(level, first, second)

    def _raise_multi_diff(self, first, second, formatter):
        first_lines = first.splitlines(True)      # keepends
        second_lines = second.splitlines(True)
        if len(first_lines) < 3 or len(second_lines) < 3:
            return
        self.log("%s\n\n!=\n\n%s" % (first.rstrip(), second.rstrip()))
        diffs = list(difflib.unified_diff(first_lines, second_lines,
                                          fromfile='first', tofile='second',
                                          lineterm=''))
        diffs[3:] = [item[0] + formatter(item[1:]).rstrip()
                     for item in diffs[3:]]
        raise AssertionError('Multiline strings are different:\n' +
                             '\n'.join(diffs))

    def _include_values(self, values):
        return is_truthy(values) and str(values).upper() != 'NO VALUES'

    def _strip_spaces(self, value, strip_spaces):
        if not is_string(value):
            return value
        if not is_string(strip_spaces):
            return value.strip() if strip_spaces else value
        if strip_spaces.upper() == 'LEADING':
            return value.lstrip()
        if strip_spaces.upper() == 'TRAILING':
            return value.rstrip()
        return value.strip() if is_truthy(strip_spaces) else value

    def _collapse_spaces(self, value):
        return re.sub(r'\s+', ' ', value) if is_string(value) else value

    def should_not_be_equal(self, first, second, msg=None, values=True,
                            ignore_case=False, strip_spaces=False,
                            collapse_spaces=False):
        """Fails if the given objects are equal.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values``.

        If ``ignore_case`` is given a true value (see `Boolean arguments`) and
        both arguments are strings, comparison is done case-insensitively.

        If ``strip_spaces`` is given a true value (see `Boolean arguments`)
        and both arguments are strings, the comparison is done without leading
        and trailing spaces. If ``strip_spaces`` is given a string value
        ``LEADING`` or ``TRAILING`` (case-insensitive), the comparison is done
        without leading or trailing spaces, respectively.

        If ``collapse_spaces`` is given a true value (see `Boolean arguments`) and both
        arguments are strings, the comparison is done with all white spaces replaced by
        a single space character.

        ``strip_spaces`` is new in Robot Framework 4.0 and ``collapse_spaces`` is new
        in Robot Framework 4.1.
        """
        self._log_types_at_info_if_different(first, second)
        if is_string(first) and is_string(second):
            if ignore_case:
                first = first.lower()
                second = second.lower()
            if strip_spaces:
                first = self._strip_spaces(first, strip_spaces)
                second = self._strip_spaces(second, strip_spaces)
            if collapse_spaces:
                first = self._collapse_spaces(first)
                second = self._collapse_spaces(second)
        self._should_not_be_equal(first, second, msg, values)

    def _should_not_be_equal(self, first, second, msg, values):
        assert_not_equal(first, second, msg, self._include_values(values))

    def should_not_be_equal_as_integers(self, first, second, msg=None,
                                        values=True, base=None):
        """Fails if objects are equal after converting them to integers.

        See `Convert To Integer` for information how to convert integers from
        other bases than 10 using ``base`` argument or ``0b/0o/0x`` prefixes.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values``.

        See `Should Be Equal As Integers` for some usage examples.
        """
        self._log_types_at_info_if_different(first, second)
        self._should_not_be_equal(self._convert_to_integer(first, base),
                                  self._convert_to_integer(second, base),
                                  msg, values)

    def should_be_equal_as_integers(self, first, second, msg=None, values=True,
                                    base=None):
        """Fails if objects are unequal after converting them to integers.

        See `Convert To Integer` for information how to convert integers from
        other bases than 10 using ``base`` argument or ``0b/0o/0x`` prefixes.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values``.

        Examples:
        | Should Be Equal As Integers | 42   | ${42} | Error message |
        | Should Be Equal As Integers | ABCD | abcd  | base=16 |
        | Should Be Equal As Integers | 0b1011 | 11  |
        """
        self._log_types_at_info_if_different(first, second)
        self._should_be_equal(self._convert_to_integer(first, base),
                              self._convert_to_integer(second, base),
                              msg, values)

    def should_not_be_equal_as_numbers(self, first, second, msg=None,
                                       values=True, precision=6):
        """Fails if objects are equal after converting them to real numbers.

        The conversion is done with `Convert To Number` keyword using the
        given ``precision``.

        See `Should Be Equal As Numbers` for examples on how to use
        ``precision`` and why it does not always work as expected. See also
        `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values``.
        """
        self._log_types_at_info_if_different(first, second)
        first = self._convert_to_number(first, precision)
        second = self._convert_to_number(second, precision)
        self._should_not_be_equal(first, second, msg, values)

    def should_be_equal_as_numbers(self, first, second, msg=None, values=True,
                                   precision=6):
        """Fails if objects are unequal after converting them to real numbers.

        The conversion is done with `Convert To Number` keyword using the
        given ``precision``.

        Examples:
        | Should Be Equal As Numbers | ${x} | 1.1 | | # Passes if ${x} is 1.1 |
        | Should Be Equal As Numbers | 1.123 | 1.1 | precision=1  | # Passes |
        | Should Be Equal As Numbers | 1.123 | 1.4 | precision=0  | # Passes |
        | Should Be Equal As Numbers | 112.3 | 75  | precision=-2 | # Passes |

        As discussed in the documentation of `Convert To Number`, machines
        generally cannot store floating point numbers accurately. Because of
        this limitation, comparing floats for equality is problematic and
        a correct approach to use depends on the context. This keyword uses
        a very naive approach of rounding the numbers before comparing them,
        which is both prone to rounding errors and does not work very well if
        numbers are really big or small. For more information about comparing
        floats, and ideas on how to implement your own context specific
        comparison algorithm, see
        http://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/.

        If you want to avoid possible problems with floating point numbers,
        you can implement custom keywords using Python's
        [http://docs.python.org/library/decimal.html|decimal] or
        [http://docs.python.org/library/fractions.html|fractions] modules.

        See `Should Not Be Equal As Numbers` for a negative version of this
        keyword and `Should Be Equal` for an explanation on how to override
        the default error message with ``msg`` and ``values``.
        """
        self._log_types_at_info_if_different(first, second)
        first = self._convert_to_number(first, precision)
        second = self._convert_to_number(second, precision)
        self._should_be_equal(first, second, msg, values)

    def should_not_be_equal_as_strings(self, first, second, msg=None, values=True,
                                       ignore_case=False, strip_spaces=False,
                                       collapse_spaces=False):
        """Fails if objects are equal after converting them to strings.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values``.

        If ``ignore_case`` is given a true value (see `Boolean arguments`),
        comparison is done case-insensitively.

        If ``strip_spaces`` is given a true value (see `Boolean arguments`)
        and both arguments are strings, the comparison is done without leading
        and trailing spaces. If ``strip_spaces`` is given a string value
        ``LEADING`` or ``TRAILING`` (case-insensitive), the comparison is done
        without leading or trailing spaces, respectively.

        If ``collapse_spaces`` is given a true value (see `Boolean arguments`) and both
        arguments are strings, the comparison is done with all white spaces replaced by
        a single space character.

        Strings are always [http://www.macchiato.com/unicode/nfc-faq|
        NFC normalized].

        ``strip_spaces`` is new in Robot Framework 4.0 and ``collapse_spaces`` is new
        in Robot Framework 4.1.
        """
        self._log_types_at_info_if_different(first, second)
        first = self._convert_to_string(first)
        second = self._convert_to_string(second)
        if ignore_case:
            first = first.lower()
            second = second.lower()
        if strip_spaces:
            first = self._strip_spaces(first, strip_spaces)
            second = self._strip_spaces(second, strip_spaces)
        if collapse_spaces:
            first = self._collapse_spaces(first)
            second = self._collapse_spaces(second)
        self._should_not_be_equal(first, second, msg, values)

    def should_be_equal_as_strings(self, first, second, msg=None, values=True,
                                   ignore_case=False, strip_spaces=False,
                                   formatter='str', collapse_spaces=False):
        """Fails if objects are unequal after converting them to strings.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg``, ``values`` and ``formatter``.

        If ``ignore_case`` is given a true value (see `Boolean arguments`),
        comparison is done case-insensitively. If both arguments are
        multiline strings, this keyword uses `multiline string comparison`.

        If ``strip_spaces`` is given a true value (see `Boolean arguments`)
        and both arguments are strings, the comparison is done without leading
        and trailing spaces. If ``strip_spaces`` is given a string value
        ``LEADING`` or ``TRAILING`` (case-insensitive), the comparison is done
        without leading or trailing spaces, respectively.

        If ``collapse_spaces`` is given a true value (see `Boolean arguments`) and both
        arguments are strings, the comparison is done with all white spaces replaced by
        a single space character.

        Strings are always [http://www.macchiato.com/unicode/nfc-faq| NFC normalized].

        ``strip_spaces`` is new in Robot Framework 4.0
        and ``collapse_spaces`` is new in Robot Framework 4.1.
        """
        self._log_types_at_info_if_different(first, second)
        first = self._convert_to_string(first)
        second = self._convert_to_string(second)
        if ignore_case:
            first = first.lower()
            second = second.lower()
        if strip_spaces:
            first = self._strip_spaces(first, strip_spaces)
            second = self._strip_spaces(second, strip_spaces)
        if collapse_spaces:
            first = self._collapse_spaces(first)
            second = self._collapse_spaces(second)
        self._should_be_equal(first, second, msg, values, formatter)

    def should_not_start_with(self, str1, str2, msg=None, values=True,
                              ignore_case=False, strip_spaces=False,
                              collapse_spaces=False):
        """Fails if the string ``str1`` starts with the string ``str2``.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values``, as well as for semantics
        of the ``ignore_case``, ``strip_spaces``, and ``collapse_spaces`` options.
        """
        if ignore_case:
            str1 = str1.lower()
            str2 = str2.lower()
        if strip_spaces:
            str1 = self._strip_spaces(str1, strip_spaces)
            str2 = self._strip_spaces(str2, strip_spaces)
        if collapse_spaces:
            str1 = self._collapse_spaces(str1)
            str2 = self._collapse_spaces(str2)
        if str1.startswith(str2):
            raise AssertionError(self._get_string_msg(str1, str2, msg, values,
                                                      'starts with'))

    def should_start_with(self, str1, str2, msg=None, values=True,
                          ignore_case=False, strip_spaces=False, collapse_spaces=False):
        """Fails if the string ``str1`` does not start with the string ``str2``.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values``, as well as for semantics
        of the ``ignore_case``, ``strip_spaces``, and ``collapse_spaces`` options.
        """
        if ignore_case:
            str1 = str1.lower()
            str2 = str2.lower()
        if strip_spaces:
            str1 = self._strip_spaces(str1, strip_spaces)
            str2 = self._strip_spaces(str2, strip_spaces)
        if collapse_spaces:
            str1 = self._collapse_spaces(str1)
            str2 = self._collapse_spaces(str2)
        if not str1.startswith(str2):
            raise AssertionError(self._get_string_msg(str1, str2, msg, values,
                                                      'does not start with'))

    def should_not_end_with(self, str1, str2, msg=None, values=True,
                            ignore_case=False, strip_spaces=False,
                            collapse_spaces=False):
        """Fails if the string ``str1`` ends with the string ``str2``.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values``, as well as for semantics
        of the ``ignore_case``, ``strip_spaces``, and ``collapse_spaces`` options.
        """
        if ignore_case:
            str1 = str1.lower()
            str2 = str2.lower()
        if strip_spaces:
            str1 = self._strip_spaces(str1, strip_spaces)
            str2 = self._strip_spaces(str2, strip_spaces)
        if collapse_spaces:
            str1 = self._collapse_spaces(str1)
            str2 = self._collapse_spaces(str2)
        if str1.endswith(str2):
            raise AssertionError(self._get_string_msg(str1, str2, msg, values,
                                                      'ends with'))

    def should_end_with(self, str1, str2, msg=None, values=True,
                        ignore_case=False, strip_spaces=False, collapse_spaces=False):
        """Fails if the string ``str1`` does not end with the string ``str2``.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values``, as well as for semantics
        of the ``ignore_case``, ``strip_spaces``, and ``collapse_spaces`` options.
        """
        if ignore_case:
            str1 = str1.lower()
            str2 = str2.lower()
        if strip_spaces:
            str1 = self._strip_spaces(str1, strip_spaces)
            str2 = self._strip_spaces(str2, strip_spaces)
        if collapse_spaces:
            str1 = self._collapse_spaces(str1)
            str2 = self._collapse_spaces(str2)
        if not str1.endswith(str2):
            raise AssertionError(self._get_string_msg(str1, str2, msg, values,
                                                      'does not end with'))

    def should_not_contain(self, container, item, msg=None, values=True,
                           ignore_case=False, strip_spaces=False,
                           collapse_spaces=False):
        """Fails if ``container`` contains ``item`` one or more times.

        Works with strings, lists, and anything that supports Python's ``in``
        operator.

        See `Should Be Equal` for an explanation on how to override the default
        error message with arguments ``msg`` and ``values``. ``ignore_case``
        has exactly the same semantics as with `Should Contain`.

        If ``strip_spaces`` is given a true value (see `Boolean arguments`)
        and both arguments are strings, the comparison is done without leading
        and trailing spaces. If ``strip_spaces`` is given a string value
        ``LEADING`` or ``TRAILING`` (case-insensitive), the comparison is done
        without leading or trailing spaces, respectively.

        If ``collapse_spaces`` is given a true value (see `Boolean arguments`) and both
        arguments are strings, the comparison is done with all white spaces replaced by
        a single space character.

        Examples:
        | Should Not Contain | ${some list} | value  |
        | Should Not Contain | ${output}    | FAILED | ignore_case=True |

        ``strip_spaces`` is new in Robot Framework 4.0 and ``collapse_spaces`` is new
        in Robot Framework 4.1.
        """
        # TODO: It is inconsistent that errors show original case in 'container'
        # 'item' is in lower case. Should rather show original case everywhere
        # and add separate '(case-insensitive)' not to the error message.
        # This same logic should be used with all keywords supporting
        # case-insensitive comparisons.
        orig_container = container
        if ignore_case and is_string(item):
            item = item.lower()
            if is_string(container):
                container = container.lower()
            elif is_list_like(container):
                container = set(x.lower() if is_string(x) else x for x in container)
        if strip_spaces and is_string(item):
            item = self._strip_spaces(item, strip_spaces)
            if is_string(container):
                container = self._strip_spaces(container, strip_spaces)
            elif is_list_like(container):
                container = set(self._strip_spaces(x, strip_spaces) for x in container)
        if collapse_spaces and is_string(item):
            item = self._collapse_spaces(item)
            if is_string(container):
                container = self._collapse_spaces(container)
            elif is_list_like(container):
                container = set(self._collapse_spaces(x) for x in container)
        if item in container:
            raise AssertionError(self._get_string_msg(orig_container, item, msg,
                                                      values, 'contains'))

    def should_contain(self, container, item, msg=None, values=True,
                       ignore_case=False, strip_spaces=False, collapse_spaces=False):
        """Fails if ``container`` does not contain ``item`` one or more times.

        Works with strings, lists, and anything that supports Python's ``in``
        operator.

        See `Should Be Equal` for an explanation on how to override the default
        error message with arguments ``msg`` and ``values``.

        If ``ignore_case`` is given a true value (see `Boolean arguments`) and
        compared items are strings, it indicates that comparison should be
        case-insensitive. If the ``container`` is a list-like object, string
        items in it are compared case-insensitively.

        If ``strip_spaces`` is given a true value (see `Boolean arguments`)
        and both arguments are strings, the comparison is done without leading
        and trailing spaces. If ``strip_spaces`` is given a string value
        ``LEADING`` or ``TRAILING`` (case-insensitive), the comparison is done
        without leading or trailing spaces, respectively.

        If ``collapse_spaces`` is given a true value (see `Boolean arguments`) and both
        arguments are strings, the comparison is done with all white spaces replaced by
        a single space character.

        Examples:
        | Should Contain | ${output}    | PASS  |
        | Should Contain | ${some list} | value | msg=Failure! | values=False |
        | Should Contain | ${some list} | value | ignore_case=True |

        ``strip_spaces`` is new in Robot Framework 4.0 and ``collapse_spaces`` is new
        in Robot Framework 4.1.
        """
        orig_container = container
        if ignore_case and is_string(item):
            item = item.lower()
            if is_string(container):
                container = container.lower()
            elif is_list_like(container):
                container = set(x.lower() if is_string(x) else x for x in container)
        if strip_spaces and is_string(item):
            item = self._strip_spaces(item, strip_spaces)
            if is_string(container):
                container = self._strip_spaces(container, strip_spaces)
            elif is_list_like(container):
                container = set(self._strip_spaces(x, strip_spaces) for x in container)
        if collapse_spaces and is_string(item):
            item = self._collapse_spaces(item)
            if is_string(container):
                container = self._collapse_spaces(container)
            elif is_list_like(container):
                container = set(self._collapse_spaces(x) for x in container)
        if item not in container:
            raise AssertionError(self._get_string_msg(orig_container, item, msg,
                                                      values, 'does not contain'))

    def should_contain_any(self, container, *items, **configuration):
        """Fails if ``container`` does not contain any of the ``*items``.

        Works with strings, lists, and anything that supports Python's ``in``
        operator.

        Supports additional configuration parameters ``msg``, ``values``,
        ``ignore_case`` and ``strip_spaces``, and ``collapse_spaces``
        which have exactly the same semantics as arguments with same
        names have with `Should Contain`. These arguments must always
        be given using ``name=value`` syntax after all ``items``.

        Note that possible equal signs in ``items`` must be escaped with
        a backslash (e.g. ``foo\\=bar``) to avoid them to be passed in
        as ``**configuration``.

        Examples:
        | Should Contain Any | ${string} | substring 1 | substring 2 |
        | Should Contain Any | ${list}   | item 1 | item 2 | item 3 |
        | Should Contain Any | ${list}   | item 1 | item 2 | item 3 | ignore_case=True |
        | Should Contain Any | ${list}   | @{items} | msg=Custom message | values=False |
        """
        msg = configuration.pop('msg', None)
        values = configuration.pop('values', True)
        ignore_case = is_truthy(configuration.pop('ignore_case', False))
        strip_spaces = configuration.pop('strip_spaces', False)
        collapse_spaces = is_truthy(configuration.pop('collapse_spaces', False))
        if configuration:
            raise RuntimeError("Unsupported configuration parameter%s: %s."
                               % (s(configuration), seq2str(sorted(configuration))))
        if not items:
            raise RuntimeError('One or more items required.')
        orig_container = container
        if ignore_case:
            items = [x.lower() if is_string(x) else x for x in items]
            if is_string(container):
                container = container.lower()
            elif is_list_like(container):
                container = set(x.lower() if is_string(x) else x for x in container)
        if strip_spaces:
            items = [self._strip_spaces(x, strip_spaces) for x in items]
            if is_string(container):
                container = self._strip_spaces(container, strip_spaces)
            elif is_list_like(container):
                container = set(self._strip_spaces(x, strip_spaces) for x in container)
        if collapse_spaces:
            items = [self._collapse_spaces(x) for x in items]
            if is_string(container):
                container = self._collapse_spaces(container)
            elif is_list_like(container):
                container = set(self._collapse_spaces(x) for x in container)
        if not any(item in container for item in items):
            msg = self._get_string_msg(orig_container,
                                       seq2str(items, lastsep=' or '),
                                       msg, values,
                                       'does not contain any of',
                                       quote_item2=False)
            raise AssertionError(msg)

    def should_not_contain_any(self, container, *items, **configuration):
        """Fails if ``container`` contains one or more of the ``*items``.

        Works with strings, lists, and anything that supports Python's ``in``
        operator.

        Supports additional configuration parameters ``msg``, ``values``,
        ``ignore_case`` and ``strip_spaces``, and ``collapse_spaces`` which have exactly
        the same semantics as arguments with same names have with `Should Contain`.
        These arguments must always be given using ``name=value`` syntax after all ``items``.

        Note that possible equal signs in ``items`` must be escaped with
        a backslash (e.g. ``foo\\=bar``) to avoid them to be passed in
        as ``**configuration``.

        Examples:
        | Should Not Contain Any | ${string} | substring 1 | substring 2 |
        | Should Not Contain Any | ${list}   | item 1 | item 2 | item 3 |
        | Should Not Contain Any | ${list}   | item 1 | item 2 | item 3 | ignore_case=True |
        | Should Not Contain Any | ${list}   | @{items} | msg=Custom message | values=False |
        """
        msg = configuration.pop('msg', None)
        values = configuration.pop('values', True)
        ignore_case = is_truthy(configuration.pop('ignore_case', False))
        strip_spaces = configuration.pop('strip_spaces', False)
        collapse_spaces = is_truthy(configuration.pop('collapse_spaces', False))
        if configuration:
            raise RuntimeError("Unsupported configuration parameter%s: %s."
                               % (s(configuration), seq2str(sorted(configuration))))
        if not items:
            raise RuntimeError('One or more items required.')
        orig_container = container
        if ignore_case:
            items = [x.lower() if is_string(x) else x for x in items]
            if is_string(container):
                container = container.lower()
            elif is_list_like(container):
                container = set(x.lower() if is_string(x) else x for x in container)
        if strip_spaces:
            items = [self._strip_spaces(x, strip_spaces) for x in items]
            if is_string(container):
                container = self._strip_spaces(container, strip_spaces)
            elif is_list_like(container):
                container = set(self._strip_spaces(x, strip_spaces) for x in container)
        if collapse_spaces:
            items = [self._collapse_spaces(x) for x in items]
            if is_string(container):
                container = self._collapse_spaces(container)
            elif is_list_like(container):
                container = set(self._collapse_spaces(x) for x in container)
        if any(item in container for item in items):
            msg = self._get_string_msg(orig_container,
                                       seq2str(items, lastsep=' or '),
                                       msg, values,
                                       'contains one or more of',
                                       quote_item2=False)
            raise AssertionError(msg)

    def should_contain_x_times(self, container, item, count, msg=None,
                               ignore_case=False, strip_spaces=False,
                               collapse_spaces=False):
        """Fails if ``container`` does not contain ``item`` ``count`` times.

        Works with strings, lists and all objects that `Get Count` works
        with. The default error message can be overridden with ``msg`` and
        the actual count is always logged.

        If ``ignore_case`` is given a true value (see `Boolean arguments`) and
        compared items are strings, it indicates that comparison should be
        case-insensitive. If the ``container`` is a list-like object, string
        items in it are compared case-insensitively.

        If ``strip_spaces`` is given a true value (see `Boolean arguments`)
        and both arguments are strings, the comparison is done without leading
        and trailing spaces. If ``strip_spaces`` is given a string value
        ``LEADING`` or ``TRAILING`` (case-insensitive), the comparison is done
        without leading or trailing spaces, respectively.

        If ``collapse_spaces`` is given a true value (see `Boolean arguments`) and both
        arguments are strings, the comparison is done with all white spaces replaced by
        a single space character.

        Examples:
        | Should Contain X Times | ${output}    | hello | 2 |
        | Should Contain X Times | ${some list} | value | 3 | ignore_case=True |

        ``strip_spaces`` is new in Robot Framework 4.0 and ``collapse_spaces`` is new
        in Robot Framework 4.1.
        """
        count = self._convert_to_integer(count)
        orig_container = container
        if is_string(item):
            if ignore_case:
                item = item.lower()
                if is_string(container):
                    container = container.lower()
                elif is_list_like(container):
                    container = [x.lower() if is_string(x) else x for x in container]
            if strip_spaces:
                item = self._strip_spaces(item, strip_spaces)
                if is_string(container):
                    container = self._strip_spaces(container, strip_spaces)
                elif is_list_like(container):
                    container = [self._strip_spaces(x, strip_spaces) for x in container]
            if collapse_spaces:
                item = self._collapse_spaces(item)
                if is_string(container):
                    container = self._collapse_spaces(container)
                elif is_list_like(container):
                    container = [self._collapse_spaces(x) for x in container]
        x = self.get_count(container, item)
        if not msg:
            msg = "'%s' contains '%s' %d time%s, not %d time%s." \
                    % (unic(orig_container), unic(item), x, s(x), count, s(count))
        self.should_be_equal_as_integers(x, count, msg, values=False)

    def get_count(self, container, item):
        """Returns and logs how many times ``item`` is found from ``container``.

        This keyword works with Python strings and lists and all objects
        that either have ``count`` method or can be converted to Python lists.

        Example:
        | ${count} = | Get Count | ${some item} | interesting value |
        | Should Be True | 5 < ${count} < 10 |
        """
        if not hasattr(container, 'count'):
            try:
                container = list(container)
            except:
                raise RuntimeError("Converting '%s' to list failed: %s"
                                   % (container, get_error_message()))
        count = container.count(item)
        self.log('Item found from container %d time%s.' % (count, s(count)))
        return count

    def should_not_match(self, string, pattern, msg=None, values=True,
                         ignore_case=False):
        """Fails if the given ``string`` matches the given ``pattern``.

        Pattern matching is similar as matching files in a shell with
        ``*``, ``?`` and ``[chars]`` acting as wildcards. See the
        `Glob patterns` section for more information.

        If ``ignore_case`` is given a true value (see `Boolean arguments`),
        the comparison is case-insensitive.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values`.
        """
        if self._matches(string, pattern, caseless=ignore_case):
            raise AssertionError(self._get_string_msg(string, pattern, msg,
                                                      values, 'matches'))

    def should_match(self, string, pattern, msg=None, values=True,
                     ignore_case=False):
        """Fails if the given ``string`` does not match the given ``pattern``.

        Pattern matching is similar as matching files in a shell with
        ``*``, ``?`` and ``[chars]`` acting as wildcards. See the
        `Glob patterns` section for more information.

        If ``ignore_case`` is given a true value (see `Boolean arguments`) and
        compared items are strings, it indicates that comparison should be
        case-insensitive.

        See `Should Be Equal` for an explanation on how to override the default
        error message with ``msg`` and ``values``.
        """
        if not self._matches(string, pattern, caseless=ignore_case):
            raise AssertionError(self._get_string_msg(string, pattern, msg,
                                                      values, 'does not match'))

    def should_match_regexp(self, string, pattern, msg=None, values=True):
        """Fails if ``string`` does not match ``pattern`` as a regular expression.

        See the `Regular expressions` section for more information about
        regular expressions and how to use then in Robot Framework test data.

        Notice that the given pattern does not need to match the whole string.
        For example, the pattern ``ello`` matches the string ``Hello world!``.
        If a full match is needed, the ``^`` and ``$`` characters can be used
        to denote the beginning and end of the string, respectively.
        For example, ``^ello$`` only matches the exact string ``ello``.

        Possible flags altering how the expression is parsed (e.g.
        ``re.IGNORECASE``, ``re.MULTILINE``) must be embedded to the
        pattern like ``(?im)pattern``. The most useful flags are ``i``
        (case-insensitive), ``m`` (multiline mode), ``s`` (dotall mode)
        and ``x`` (verbose).

        If this keyword passes, it returns the portion of the string that
        matched the pattern. Additionally, the possible captured groups are
        returned.

        See the `Should Be Equal` keyword for an explanation on how to override
        the default error message with the ``msg`` and ``values`` arguments.

        Examples:
        | Should Match Regexp | ${output} | \\\\d{6}   | # Output contains six numbers  |
        | Should Match Regexp | ${output} | ^\\\\d{6}$ | # Six numbers and nothing more |
        | ${ret} = | Should Match Regexp | Foo: 42 | (?i)foo: \\\\d+ |
        | ${match} | ${group1} | ${group2} = |
        | ...      | Should Match Regexp | Bar: 43 | (Foo|Bar): (\\\\d+) |
        =>
        | ${ret} = 'Foo: 42'
        | ${match} = 'Bar: 43'
        | ${group1} = 'Bar'
        | ${group2} = '43'
        """
        res = re.search(pattern, string)
        if res is None:
            raise AssertionError(self._get_string_msg(string, pattern, msg,
                                                      values, 'does not match'))
        match = res.group(0)
        groups = res.groups()
        if groups:
            return [match] + list(groups)
        return match

    def should_not_match_regexp(self, string, pattern, msg=None, values=True):
        """Fails if ``string`` matches ``pattern`` as a regular expression.

        See `Should Match Regexp` for more information about arguments.
        """
        if re.search(pattern, string) is not None:
            raise AssertionError(self._get_string_msg(string, pattern, msg,
                                                      values, 'matches'))

    def get_length(self, item):
        """Returns and logs the length of the given item as an integer.

        The item can be anything that has a length, for example, a string,
        a list, or a mapping. The keyword first tries to get the length with
        the Python function ``len``, which calls the  item's ``__len__`` method
        internally. If that fails, the keyword tries to call the item's
        possible ``length`` and ``size`` methods directly. The final attempt is
        trying to get the value of the item's ``length`` attribute. If all
        these attempts are unsuccessful, the keyword fails.

        Examples:
        | ${length} = | Get Length    | Hello, world! |        |
        | Should Be Equal As Integers | ${length}     | 13     |
        | @{list} =   | Create List   | Hello,        | world! |
        | ${length} = | Get Length    | ${list}       |        |
        | Should Be Equal As Integers | ${length}     | 2      |

        See also `Length Should Be`, `Should Be Empty` and `Should Not Be
        Empty`.
        """
        length = self._get_length(item)
        self.log('Length is %d' % length)
        return length

    def _get_length(self, item):
        try:
            return len(item)
        except RERAISED_EXCEPTIONS:
            raise
        except:
            try:
                return item.length()
            except RERAISED_EXCEPTIONS:
                raise
            except:
                try:
                    return item.size()
                except RERAISED_EXCEPTIONS:
                    raise
                except:
                    try:
                        return item.length
                    except RERAISED_EXCEPTIONS:
                        raise
                    except:
                        raise RuntimeError("Could not get length of '%s'." % item)

    def length_should_be(self, item, length, msg=None):
        """Verifies that the length of the given item is correct.

        The length of the item is got using the `Get Length` keyword. The
        default error message can be overridden with the ``msg`` argument.
        """
        length = self._convert_to_integer(length)
        actual = self.get_length(item)
        if actual != length:
            raise AssertionError(msg or "Length of '%s' should be %d but is %d."
                                        % (item, length, actual))

    def should_be_empty(self, item, msg=None):
        """Verifies that the given item is empty.

        The length of the item is got using the `Get Length` keyword. The
        default error message can be overridden with the ``msg`` argument.
        """
        if self.get_length(item) > 0:
            raise AssertionError(msg or "'%s' should be empty." % (item,))

    def should_not_be_empty(self, item, msg=None):
        """Verifies that the given item is not empty.

        The length of the item is got using the `Get Length` keyword. The
        default error message can be overridden with the ``msg`` argument.
        """
        if self.get_length(item) == 0:
            raise AssertionError(msg or "'%s' should not be empty." % (item,))

    def _get_string_msg(self, item1, item2, custom_message, include_values,
                        delimiter, quote_item1=True, quote_item2=True):
        if custom_message and not self._include_values(include_values):
            return custom_message
        item1 = "'%s'" % unic(item1) if quote_item1 else unic(item1)
        item2 = "'%s'" % unic(item2) if quote_item2 else unic(item2)
        default_message = '%s %s %s' % (item1, delimiter, item2)
        if not custom_message:
            return default_message
        return '%s: %s' % (custom_message, default_message)


class BuiltIn(_Verify, _Converter):
        Nothing = True
