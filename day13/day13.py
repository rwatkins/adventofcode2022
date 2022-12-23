import functools
import itertools
import json
import sys


def types(l, r):
    return type(l), type(r)


def compare(left, right, indent=0, debug=False):
    for l, r in itertools.zip_longest(left, right):
        if l is None:
            return True
        if r is None:
            return False
        if types(l, r) == (int, list):
            l = [l]
        if types(l, r) == (list, int):
            r = [r]

        if types(l, r) == (list, list):
            result = compare(l, r, indent=indent + 2, debug=debug)
            if result is not None:
                return result
        elif types(l, r) == (int, int):
            if l < r:
                return True
            if l > r:
                return False
        else:
            raise Exception(f"Unhandled types: l={type(l)} r={type(r)}")

    return None


def part1(input_str):
    pairs = [p.split("\n") for p in input_str.split("\n\n")]
    pairs = [tuple(json.loads(pp) for pp in p) for p in pairs]
    return sum(i for i, (left, right) in enumerate(pairs, 1) if compare(left, right))


def part2(input_str):
    input_str += "\n[[2]]\n[[6]]"
    lines = [json.loads(line) for line in input_str.split("\n") if line]
    lines.sort(key=functools.cmp_to_key(lambda a, b: -1 if compare(a, b) else 1))
    indexes = [k for k, v in enumerate(lines, 1) if v == [[2]] or v == [[6]]]
    return indexes[0] * indexes[1]


if __name__ == "__main__":
    with open(sys.argv[1]) as f:
        input_str = f.read().strip()

    print("Part 1:", part1(input_str))
    print("Part 2:", part2(input_str))
