#!/usr/bin/env python
import os
import string
import time
import csv
from subprocess import Popen, call, PIPE

langs=['java', 'scala', 'stm', 'scoop', 'scoop_poc']
tasks=['condition', 'mutex', 'noshare', 'prodcons', 'share']
inputs={'mutex': '20000',
        'condition': '5000',
        'noshare': '',
        'prodcons': '20000',
        'share': '5000'
        }

def make_command(lang, task, num_workers):
    return './run.sh ' + inputs[task] + ' ' + str(num_workers)

def run(results, task, lang, num_workers):
    t1 = time.time ()
    
    os.chdir (os.path.join(lang, task))

    proc = Popen (make_command (lang, task, num_workers), 
                  stdout=PIPE, stderr=PIPE, 
                  shell=True)
    (out,err) = proc.communicate ()
    
    os.chdir (os.path.join ('..', '..'))
    t2 = time.time ()

    # scoop has a wrapper to restart it if it deadlocks
    # so we have to use its output (the time).
    if lang == 'scoop':
        # there may be many crashes first, so we just take the
        # last line, which is quoted, so we strip that out.
        lastline = string.split(err.strip(),'\n')[-1].replace('"','')
        tdiff = float(lastline)
    elif lang == 'scala':
        lastline = string.split(out.strip(),'\n')[-1]
        tdiff = float(lastline)
    else:
        tdiff = t2 - t1

    data = [task, lang, num_workers, tdiff]
    results.append(data)
    print (data)

def main():
    results = []

    for task in tasks:
        for lang in langs:
            for thread in [1,2,4,8,16,32]:
                for i in range(3):
                    run(results, task,lang, thread)

    with open('perf_results.csv', 'wb') as csv_file:
        perfwriter = csv.writer(csv_file, quoting=csv.QUOTE_MINIMAL)
        perfwriter.writerow(['Task', 'Language', 'Threads', 'Time'])
        for result in results:
            perfwriter.writerow(result)

if __name__ == "__main__":
    main()
