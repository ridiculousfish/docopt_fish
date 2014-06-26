#!/usr/bin/python

import re
import json
import subprocess
import sys

executable = "./run_testcase"

def parse_test(raw):
    raw = re.compile('#.*$', re.M).sub('', raw).strip()
    if raw.startswith('"""'):
        raw = raw[3:]

    for fixture in raw.split('r"""'):
        name = ''
        doc, _, body = fixture.partition('"""')
        #print "@@" + doc + " \n&&" + body
        cases = []
        for case in body.split('$')[1:]:
            argv, _, expect = case.strip().partition('\n')
            expect = json.loads(expect)
            prog, _, argv = argv.strip().partition(' ')
            cases.append((prog, argv, expect))
            #print "%%" + argv + " -- " + str(expect)

        yield name, doc, cases

failures = 0
passes = 0

tests = open('testcases.docopt','r').read()
parsed_tests = [x for x in parse_test(tests)]
testnum = 0
for _, doc, cases in parsed_tests:
    testnum += 1

    # Allow the user to specify the test numbers to run
    if len(sys.argv) > 1 and str(testnum) not in sys.argv:
        continue

    if not cases: continue

    testcasenum = 0
    for prog, argv, expect in cases:
        testcasenum += 1
        print "Test %d.%d / %d" % (testnum, testcasenum, len(parsed_tests))
        args = [ x for x in argv.split() if x ]

        print "argv: " + ' '.join(args)
        print "expect: " + str(expect)

        expect_error = not isinstance(expect, dict)

        error = None
        out = None
        #print " ".join([executable, doc] + args)
        proc = subprocess.Popen([executable] + args, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        out, _ = proc.communicate(doc)
        retcode = proc.returncode
        if retcode != 0:
            if not expect_error:
                error = "\n ** Test %d.%d should have succeeded! exit code = %s" % (testnum, testcasenum, retcode)
        elif expect_error:
            error = " ** an error was expected but it appeared to succeed!"
        else:
            # Process returned success as expected
            json_out = json.loads(out)
            if expect != json_out:
                error = " ** JSON does not match expected: %r" % expect

        if not error:
            passes += 1
            continue

        failures += 1

        print ""
        print "="*10 + "Input" + "="*10
        print doc
        print "="*10 + "Args" + "="*10
        print ' '.join(args)
        print '='*10 + 'Output' + "="*10
        print prog, argv
        print '='*20
        if out:
            print out
        print error
        sys.exit(0)

if failures:
    print "%d failures" % failures
else:
    print "PASS (%d)" % passes
