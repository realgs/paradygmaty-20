# from copy import deepcopy
from typing import Iterable, Sized


def quick_sort(array: Iterable and Sized):
    def quick_sort(array: Iterable and Sized, left: int, right: int) -> None:
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
    

def main():
    array_1 = [7, 10, 20, 13, 16]
    quick_sort(array_1)
    print(f'Array sorted by quicksort: {array_1}')


if __name__ == '__main__':
    main()
