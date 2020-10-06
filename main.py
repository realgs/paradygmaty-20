from copy import deepcopy
from typing import Iterable, Sized, NoReturn


def quick_sort(array: Iterable and Sized) -> None:
    def quick_sort(array: Iterable and Sized, left: int, right: int) -> NoReturn:
        if left < right:
            position = left - 1
            pivot = array[right]
            for j in range(left, right):
                if array[j] < pivot:
                    position += 1
                    array[position], array[j] = array[j], array[position]
            array[position + 1], array[right] = array[right], array[position + 1]
            quick_sort(array, left, position - 1)
            quick_sort(array, position + 1, right)
    quick_sort(array, 0, len(array) - 1)


def counting_sort(array: Iterable[int] and Sized) -> NoReturn:
    buckets = [0 for i in range(max(array) + 1)]
    for i in array:
        buckets[i] += 1
    pointer = 0
    for i, bucket in enumerate(buckets):
        if bucket >= 0:
            for _ in range(bucket):
                array[pointer] = i
                pointer += 1


def main():
    array_1 = [7, 10, 20, 13, 16]
    array_2 = deepcopy(array_1)
    print(f'Base array: {array_1}')
    quick_sort(array_1)
    print(f'Array sorted by quicksort: {array_1}')
    counting_sort(array_2)
    print(f'Array sorted by couting sort: {array_2}')


if __name__ == '__main__':
    main()
