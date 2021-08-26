import time
from rflibs import OSLIB, RFBASE, COLECT, STRING, DT, PROLIB

# Test OSLIB: PeratingSystem
OSLIB.create_file('a_test_file', "文件中的第一行 \n文件中的第二行")
OSLIB.remove_file("a_test_file")
OSLIB.should_not_exist("a_test_file")
a_word = "I Love LIBs"
rc, output = OSLIB.run_and_return_rc_and_output(f"echo {a_word}")
assert rc == 0
assert output == a_word

# Test RFBASE: BuiltIn
a_int = RFBASE.convert_to_integer("12345")
assert a_int == 12345
RFBASE.should_not_be_true("11"=="22", "11 不等于 22")

# Test CLECT: Collections
a_list = ["baby"]
COLECT.append_to_list(a_list, "affair", "boy", "blue", "adult", "cut", "fool")
COLECT.should_contain_match(a_list, "a*", "列表里面应该包含a开头的单词")

# Test STRING: String
a_str = "'[a string in json]'"
clean_str = STRING.remove_string(a_str, '"', "'", '[', ']')
assert clean_str == "a string in json"

# Test DT: DateTime
sub = DT.subtract_date_from_date("2014-05-28 12:05:52", "2014-05-28 12:05:12")
assert sub == 40
a_week_later = DT.add_time_to_date("2014-05-28 12:05:03.111", "7 days")
assert a_week_later == "2014-06-04 12:05:03.111"

# Test PROLIB: Process
res = PROLIB.run_process("python", "-c", "print('Hello world')")
assert res.stdout == "Hello world"
assert res.rc == 0

p1 = PROLIB.start_process("sleep 10", shell=True, alias="mysleep1")
p2 = PROLIB.start_process("sleep 2", shell=True, alias="mysleep2")
PROLIB.process_should_be_running("mysleep1")
PROLIB.terminate_process("mysleep1")
PROLIB.process_should_be_stopped("mysleep1")
PROLIB.wait_for_process(handle="mysleep2", timeout="3s", on_timeout="kill")  # Caution: Do not use time.sleep
res1 = PROLIB.get_process_result("mysleep1")
res2 = PROLIB.get_process_result("mysleep2")
assert res1.rc == -15
assert res2.rc == 0
