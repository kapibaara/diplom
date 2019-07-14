import os
from mako.template import Template
import codecs
from scipy.interpolate import interp1d
import numpy as np
import matplotlib.pyplot as plt
from scripts.getera import Getera
from scripts.getera_graphics import read_file, get_burn_time, plot_spline

class Sketch():

    def __init__(self, n_per_pel=0, n_cen_pel=0):
        self.cur_path = os.path.split(__file__)[0]
        self.sketch_exe_path = r"C:\Users\kapib\Documents\Repositories\diplom\desing of fpu\sketch_fast_reactor\SKETCH.exe"

        self.getera_outputs_path = os.path.join(self.cur_path, r"../Getera-93/prakticeOutput/")
        self.getera_outputs = os.listdir(self.getera_outputs_path)
        self.sketch_input = os.path.join(self.cur_path, r"../sketch_fast_reactor/Input/prakticeInput.dat")

        self.sketch_template_path = os.path.join(self.cur_path, r".\sketch.mako")

        self.n_per_not_pel = 10 - n_per_pel
        self.n_per_pel = n_per_pel
        self.n_cen_not_pel = 10 - n_cen_pel
        self.n_cen_pel = n_cen_pel

    def read_file(self, file):
        lines = []
        with open(file, 'r') as f:
            lines = f.readlines()
        return lines

    def get_sketch_input(self):
        getera_outputs_data = {}

        for getera_output in self.getera_outputs:
            name = getera_output.split(".")[0]
            getera_outputs_data[name] = {}

            getera_output_path = os.path.join(self.cur_path, self.getera_outputs_path, getera_output)
            output_data = self.read_file(getera_output_path)
            getera_outputs_data[name] = self.get_getera_output_data(output_data)
        return getera_outputs_data

    def get_getera_output_data(self, lines):
        sections_identifier = r"*grp*flux 1/cm2c"
        matrix_identifier = r"i / j -->"
        kinf_identifier = r"***k(inf)="

        for i, line in enumerate(lines):
            if kinf_identifier in line:
                splited = line.split(kinf_identifier)
                if splited[1].startswith(" "):
                    kinf = splited[1].split(" ")[1]
                else:
                    kinf = splited[1].split(" ")[0]
            if sections_identifier in line:
                # быстрая область
                sections = lines[i + 1].split()
                d1 = sections[6]
                sa1 = sections[3]
                sf1 = sections[4]
                nsf1 = sections[5]

                # тепловая область
                sections = lines[i + 2].split()
                d2 = sections[6]
                sa2 = sections[3]
                sf2 = sections[4]
                nsf2 = sections[5]
            if matrix_identifier in line:
                matrix_row = lines[i + 1].split(" ")
                sd11 = matrix_row[16]
                sd12 = matrix_row[21].split("\n")[0]

                matrix_row = lines[i + 2].split(" ")
                sd21 = matrix_row[16]
                sd22 = matrix_row[17]

        return {
            "kinf": kinf,
            "d1": d1,
            "sa1": sa1,
            "sf1": sf1,
            "nsf1": nsf1,
            "d2": d2,
            "sa2": sa2,
            "sf2": sf2,
            "nsf2": nsf2,
            "sd11": sd11,
            "sd12": sd21,
            "sd21": sd12,
            "sd22": sd22,
        }

    def render(self):
        data = self.get_sketch_input()

        template = Template(filename=self.sketch_template_path)

        with codecs.open(self.sketch_input, 'w', encoding='cp1251') as file:
            file.write(template.render(
                data=data,
                n_per_not_pel=self.n_per_not_pel,
                n_per_pel=self.n_per_pel,
                n_cen_not_pel=self.n_cen_not_pel,
                n_cen_pel=self.n_cen_pel
            ))

    def start(self):
        self.render()

        sketch_folder, sketch_exe = os.path.split(self.sketch_exe_path)
        cur_dir = os.getcwd()
        os.chdir(sketch_folder)
        os.system(sketch_exe)
        os.chdir(cur_dir)

    def get_keff(self, lines):
        keff_separator = r"k_ef                                  :"
        keff = []

        for line in lines:
            if keff_separator in line:
                splited = line.split()
                keff.append(splited[2])
        return float(keff[1])

    def get_per_pels_ratio(self, lines):
        per_pels_separator = r"## nb = 2"

        for line in lines:
            if per_pels_separator in line:
                splited = line.split()
        return float(splited[2].split('*')[0])

    def get_cen_pels_ratio(self, lines):
        cen_pels_separator = r"## nb = 1"

        for line in lines:
            if cen_pels_separator in line:
                splited = line.split()
        return float(splited[2].split('*')[0])

def write_file(file, lines):
    with open(file, 'w') as f:
        f.write(lines)

if __name__ == "__main__":
    cur_path = os.path.split(__file__)[0]
    template_names = os.listdir("../templates")
    sketch_input = os.path.join(cur_path, r"../sketch_fast_reactor/Output/SKETCH.lst")
    getera_out_path = os.path.join(cur_path, r"../Getera-93/prakticeOutput/")
    getera_output_files = os.listdir(getera_out_path)
    getera_output_file = os.path.join(cur_path, getera_out_path, getera_output_files[2])
    keff_output_file = os.path.join(cur_path, r"./results/keff.txt")
    burn_output_file = os.path.join(cur_path, r"./results/burn.txt")
    time_output_file = os.path.join(cur_path, r"./results/time.txt")
    per_pel_ratio_output_file = os.path.join(cur_path, r"./results/per_pel_ratio.txt")
    cen_pel_ratio_output_file = os.path.join(cur_path, r"./results/cen_pel_ratio.txt")

    burn_n = 150
    keff = []
    burn = []
    per_pel_ratio = []
    cen_pel_ratio = []

    n_per_pel = 8
    n_cen_pel = 8

    for n in range(burn_n):
        burn.append(n)
        for template_name in template_names:
            getera = Getera(template_name, n, 0)
            getera.start()
        sketch = Sketch(n_per_pel, n_cen_pel)
        sketch.start()
        output_lines = sketch.read_file(sketch_input)
        input_lines = sketch.read_file(sketch.sketch_input)

        keff_inprogress = sketch.get_keff(output_lines)
        keff.append(keff_inprogress)
        per_pel_ratio.append(sketch.get_per_pels_ratio(input_lines))
        cen_pel_ratio.append(sketch.get_cen_pels_ratio(input_lines))

        if ((keff_inprogress < 1.02) and (n_per_pel > 0) and ( n_cen_pel > 0)):
            n_per_pel = n_per_pel - 1
            n_cen_pel = n_cen_pel - 1
        # if ((keff_inprogress > 1.03) and (n_per_pel > 0) and ( n_cen_pel > 0)):
        #     n_per_pel = n_per_pel + 1
        #     n_cen_pel = n_cen_pel + 1
        if ((n_per_pel == 0) and ( n_cen_pel == 0)):
            break


    write_file(keff_output_file, " ".join(map(str, keff)))
    write_file(burn_output_file, " ".join(map(str, burn)))
    write_file(per_pel_ratio_output_file, " ".join(map(str, per_pel_ratio)))
    write_file(cen_pel_ratio_output_file, " ".join(map(str, cen_pel_ratio)))
    data = read_file(getera_output_file)
    time = get_burn_time(data)
    write_file(time_output_file, " ".join(map(str, time)))
    plot_spline(time, keff,"t, сутки", "keff")
