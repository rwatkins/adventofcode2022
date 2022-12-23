from pprint import pprint
import sys
from heapq import heappush, heappop


def build_grid(s):
    return [[c for c in line] for line in s.strip().splitlines()]


def find_pos(grid, char):
    for i, row in enumerate(grid):
        for j, c in enumerate(row):
            if c == char:
                return i, j


def find_start(grid):
    return find_pos(grid, "S")


def find_end(grid):
    return find_pos(grid, "E")


def reachable(a, b):
    replace = {"S": "a", "E": "z"}
    a = replace.get(a, a)
    b = replace.get(b, b)
    return ord(b) - ord(a) <= 1


def neighbors(grid, i, j):
    coords = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for ii, jj in coords:
        row, col = i + ii, j + jj
        if not (0 <= row < len(grid) and 0 <= col < len(grid[0])):
            continue

        if not reachable(grid[i][j], grid[row][col]):
            continue

        yield row, col


def reverse_neighbors(grid, i, j):
    coords = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for ii, jj in coords:
        row, col = i + ii, j + jj
        if not (0 <= row < len(grid) and 0 <= col < len(grid[0])):
            continue

        if not reachable(grid[row][col], grid[i][j]):  # reverse the order of arguments
            continue

        yield row, col


def part1(input_str):
    """
    Find the shortest path from 'S' to 'E'
    """
    grid = build_grid(input_str)
    starti, startj = find_start(grid)
    heap = [(0, starti, startj)]
    visited = [[False] * len(grid[0]) for _ in range(len(grid))]
    while True:
        steps, i, j = heappop(heap)
        if visited[i][j]:
            continue

        if grid[i][j] == "E":
            return steps

        visited[i][j] = True
        for ii, jj in neighbors(grid, i, j):
            heappush(heap, (steps + 1, ii, jj))


def part2(input_str):
    """
    Find the shortest path from any 'a' to 'E'
    """
    grid = build_grid(input_str)
    # We'll start at the end and work backward to the first 'a' we encounter
    endi, endj = find_end(grid)
    heap = [(0, endi, endj)]
    visited = [[False] * len(grid[0]) for _ in range(len(grid))]
    while True:
        steps, i, j = heappop(heap)
        if visited[i][j]:
            continue

        if grid[i][j] in ["a", "S"]:
            return steps

        visited[i][j] = True
        for ii, jj in reverse_neighbors(grid, i, j):
            heappush(heap, (steps + 1, ii, jj))


with open(sys.argv[1]) as f:
    input = f.read().strip()

print("Part 1:", part1(input))
print("Part 2:", part2(input))
