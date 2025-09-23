def collatz_sequence(n: int) -> tuple[int, int]:
    max_len: int = 0
    max_num: int = 0
    for i in range(1, n):
        count: int = 1
        temp: int = i
        while temp != 1:
            if temp % 2 == 0:
                temp //= 2
            else:
                temp = 3*temp + 1
            count += 1
        if count > max_len:
            max_num = i
            max_len = count
    return max_num, max_len


if __name__ == "__main__":
    print(collatz_sequence(10**6))