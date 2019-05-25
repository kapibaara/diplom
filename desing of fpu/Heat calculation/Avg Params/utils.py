def fill_dict_from_file(dict, file_name):
    file = open(file_name, 'r')

    for line in file:
        if line.strip() == '':
            continue
        splited_line = line.split(":")
        key = splited_line[0].strip()
        value = float(splited_line[1]
          .strip()
          .split(" ")[0]
          .strip())
        dict[key] = value

    file.close()
