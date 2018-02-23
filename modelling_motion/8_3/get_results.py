"""
Construct an iterative model of motion
"""

import argparse

def get_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("-m", "--max-t", type=float, help="maximum value to go up to", default=2)
    parser.add_argument("-d", "--delta-t", type=float, help="delta value for t", default=0.1)
    return parser.parse_args()

if __name__ == "__main__":
    args = get_args()

    max_t = args.max_t
    t_delta = args.delta_t

    a = 9.8
    v = 0
    s = 0
    t = 0
    k = 0.3

    while t < max_t:
        print("\t".join(map(str, (t, a, v, s))))
        t += t_delta
        a, v, s = 9.8 - k * v ** 2, v + a * t_delta, s + v * t_delta
