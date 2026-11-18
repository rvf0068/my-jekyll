import random

def generate_random_list(length, min_val=-10, max_val=10):
    """Generates a list of random integers of the specified length."""
    return [random.randint(min_val, max_val) for _ in range(length)]

def max_subarray_bruteforce(A):
    n = len(A)
    if n == 0:
        return 0, -1, -1  
    max_sum = float('-inf')
    best_start = -1
    best_end = -1
    for i in range(n):
        for j in range(i, n):
            current_sum = 0
            for k in range(i, j + 1):
                current_sum += A[k]
            if current_sum > max_sum:
                max_sum = current_sum
                best_start = i
                best_end = j
    return max_sum, best_start, best_end

def max_subarray_quadratic(A):
    n = len(A)
    if n == 0:
        return 0, -1, -1
    max_sum = float('-inf')
    best_start = -1
    best_end = -1
    for i in range(n):
        current_sum = 0
        for j in range(i, n):
            current_sum += A[j]
            if current_sum > max_sum:
                max_sum = current_sum
                best_start = i
                best_end = j
    return max_sum, best_start, best_end

def max_subarray_divide_and_conquer(A):
    if not A:
        return 0, -1, -1
    return _max_subarray_recursive(A, 0, len(A) - 1)

def _max_crossing_subarray(A, low, mid, high):
    """
    Finds the maximum subarray that crosses the midpoint.
    Returns: (sum, start_index, end_index)
    """
    # 1. Left Side: Scan from mid down to low
    left_sum = float('-inf')
    current_sum = 0
    max_left_idx = mid
    for i in range(mid, low-1, -1):
        current_sum += A[i]
        if current_sum > left_sum:
            left_sum = current_sum
            max_left_idx = i
    # 2. Right Side: Scan from mid+1 up to high
    right_sum = float('-inf')
    current_sum = 0
    max_right_idx = mid + 1
    for j in range(mid + 1, high + 1):
        current_sum += A[j]
        if current_sum > right_sum:
            right_sum = current_sum
            max_right_idx = j

    # The crossing sum is the combination of the best left and best right parts
    return (left_sum + right_sum, max_left_idx, max_right_idx)

def _max_subarray_recursive(A, low, high):
    if low == high:
        return A[low], low, high
    mid = (low + high) // 2
    left_result = _max_subarray_recursive(A, low, mid)
    right_result = _max_subarray_recursive(A, mid + 1, high)
    cross_result = _max_crossing_subarray(A, low, mid, high)
    return max(left_result, right_result, cross_result, key=lambda x: x)

def max_subarray_kadane(A):
    n = len(A)
    if n == 0:
        return 0, -1, -1
    max_so_far = A[0]
    current_max = A[0]
    start_index = 0
    end_index = 0
    temp_start_index = 0
    for i in range(1, n):
        if A[i] > current_max + A[i]:
            current_max = A[i]
            temp_start_index = i  
        else:
            current_max += A[i]
        if current_max > max_so_far:
            max_so_far = current_max
            start_index = temp_start_index
            end_index = i
    return max_so_far, start_index, end_index

max_subarray_kadane(data)

