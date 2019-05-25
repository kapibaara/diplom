import utils
import math

def avg_temp_calc():
    t_avg = data["Tt"] - data["ql"] / (8 * math.pi * data["lambdaT"])
    print("avg_t: {} K".format(t_avg))
    return t_avg

def avg_q_calc():
    avg_q = data["Q"] / data["Vtop"]
    print("avg_q: {} Vt/m^3".format(avg_q * 1e-6))
    return avg_q

def tau_calc():
    avg_temp = avg_temp_calc()
    avg_q = avg_q_calc()

    tau = math.pow(
        (data["Tmax"] - avg_temp) * data["ro"] * data["c"] \
            * data["kV"] / (avg_q * 8.125 * 1e-2),
        1.25
    )
    print("tau: {} min".format(tau / 60))

if __name__ == "__main__":
    data = {}
    utils.fill_dict_from_file(data, "input.txt")

    tau_calc()
