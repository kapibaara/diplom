from keff_calc import read_file, write_file

GETERA_OUTPUT_FILE = "../getera/praktice_1H2O.out"
OUTPUT_FILE_PU = "output_pu39.txt"
OUTPUT_FILE_TIME = "output_time.txt"

def get_pu39_conc(lines):
    pu39_identifier = "      pu39" # 23 pu39
    pu39_output = []

    for line in lines:
        if pu39_identifier in line:
            splited = line.split(" ")
            print(splited)
            pu39_conc = splited[8] # 8 || 7
            pu39_output.append(pu39_conc)

    # фильтрую, чтобы избавиться от повторений в массиве
    # тк при расчете с перегрузкой D2O два раза вычисляется
    # выгорание => конц Pu39 одинакова pu39_output[::2]

    return pu39_output[::2][::2]

def get_time_from_start(lines):
    time_identifier = "time from start"
    times = []

    for line in lines:
        if time_identifier in line:
            splited = line.split(" ")
            time = splited[11]
            times.append(time)
    return times[::2][::2][::2]

def format_list(list):
    formated_list = []
    for item in list:
        formated_list.append("{}\n".format(item))
    return formated_list

if __name__ == "__main__":
    getera_output = read_file(GETERA_OUTPUT_FILE)

    pu39_output = get_pu39_conc(getera_output)
    time_output = get_time_from_start(getera_output)

    write_file(OUTPUT_FILE_TIME, format_list(time_output))
    write_file(OUTPUT_FILE_PU, format_list(pu39_output))