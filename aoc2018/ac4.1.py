#! /usr/bin/env python

import re

def get_days_in(month):
    if month == 2:
        return 28
    if month in (1, 3, 5, 7, 8, 10, 12):
        return 31
    return 30

def get_dates():
    (month, day) = (2, 4)
    while (month, day) <= (11, 23):
        yield (month, day)
        day += 1
        if day > get_days_in(month):
            month += 1
            day = 1

def get_minutes():
    for (month, day) in get_dates():
        for minute in range(60):
            yield (month, day, 0, minute, 'z')

def get_sorted_input():
    with open('ac4.in.txt') as f:
        lines = sorted(list(f))
    return lines

def get_parsed_input():
    for line in get_sorted_input():
        m = re.match(r'\[1518-(\d\d)-(\d\d) (\d\d):(\d\d)\] (.*)', line)
        (month, day, hour, minute, note) = m.group(1, 2, 3, 4, 5)
        yield (int(month), int(day), int(hour), int(minute), note)

def get_merged_input():
    return sorted(list(get_parsed_input()) + list(get_minutes()))

def process_input():
    guard_num = 0
    sleeping = False
    guard_minutes = {}  # guard -> minute -> array of number of times asleep then
    guard_total_minutes = {}  # guard -> total sleeptime
    for (month, day, hour, minute, note) in get_merged_input():
        if 'begins shift' in note:
            m = re.match(r'Guard #(\d+) begins shift', note)
            guard_num = int(m.group(1))
            sleeping = False
        elif 'falls asleep' in note:
            sleeping = True
        elif 'wakes' in note:
            sleeping = False
        elif note == 'z':
            guard_minutes.setdefault(guard_num, [0] * 60)
            guard_total_minutes.setdefault(guard_num, 0)
            if sleeping:
                guard_minutes[guard_num][minute] += 1
                guard_total_minutes[guard_num] += 1
    (max_sleep, max_sleep_guard) = max(
        (s, g) for (g, s) in guard_total_minutes.items())
    (ignored, max_sleep_minute) = max(
        (s, m) for (m, s) in enumerate(guard_minutes[max_sleep_guard]))
    print((max_sleep_guard, max_sleep_minute))
    print(max_sleep_guard*max_sleep_minute)


if __name__ == '__main__':
    process_input()
