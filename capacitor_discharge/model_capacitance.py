from argparse import ArgumentParser

def get_args():
    parser = ArgumentParser()
    parser.add_argument(
        "--capacitance", "-c", type=float, help="capacitance value",
        required=True)
    parser.add_argument(
        "--resistance", "-r", type=float, help="resistance value",
        required=True)
    parser.add_argument(
        "--delta-t", "-dt", type=float, help="delta-t value", required=True)
    parser.add_argument(
        "--time", "-t", type=float, help="total time value", required=True)
    parser.add_argument(
        "--voltage", "-pd", type=float, help="initial potential difference")
    return parser.parse_args()

def make_table(C, R, dt, t, V):
    print("t/s\tQ/C")
    curr_t = 0
    Q = C * V
    while curr_t < t:
        print("{:.0f}\t{:.1e}".format(curr_t, Q))
        curr_t += dt
        Q -= Q / (R * C) * dt
    print("{:.0f}\t{:.1e}".format(curr_t, Q))

if __name__ == "__main__":
    args = get_args()
    make_table(
        args.capacitance, args.resistance, args.delta_t, args.time,
        args.voltage)
