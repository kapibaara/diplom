import os
from mako.template import Template
import codecs

class Sketch():

    def __init__(self):
        self.cur_path = os.path.split(__file__)[0]
        self.getera_outputs_path = os.path.join(self.cur_path, r"../Getera-93/prakticeOutput/")
        self.getera_outputs = os.listdir(self.getera_outputs_path)
        self.sketch_input = os.path.join(self.cur_path, r"../sketch_fast_reactor/Input/prakticeInput.dat")

        self.sketch_template_path = os.path.join(self.cur_path, r"..\templates\sketch.mako")

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
                sections = lines[i + 1].split(" ")
                d1 = sections[18]
                sa1 = sections[14]
                sf1 = sections[15]
                nsf1 = sections[16]

                # тепловая область
                sections = lines[i + 2].split(" ")
                d2 = sections[22]
                sa2 = sections[11]
                sf2 = sections[16]
                nsf2 = sections[17]
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
            "sd12": sd12,
            "sd21": sd21,
            "sd22": sd22,
        }

    def render(self):
        data = self.get_sketch_input()

        template = Template(filename=self.sketch_template_path)
        input_file = template.render(data=data)

        with codecs.open(self.sketch_input, 'w', encoding='cp1251') as file:
            file.write(template.render(data=data))



sketch = Sketch()
sketch.render()
