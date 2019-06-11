import os
from mako.template import Template
from scripts.materials import B4C, Fuel, H2O, Shall, Shall_B4C, Gd2o3


class Getera():

    def __init__(self, template_name, burn_n=0, burn_time=50, C=6E-3):
        self.cur_path = os.path.split(__file__)[0]
        self.getera_exe_path = r"C:\Users\kapib\Documents\Repositories\diplom\desing of fpu\Getera-93\Getera-93.bat"

        self.template_name = template_name
        self.filename = self.template_name.split(".")[0]
        self.template_path = os.path.join(self.cur_path, r"..\templates", self.template_name)

        self.cfg_template_path = os.path.join(self.cur_path, r".\CONFIG.mako")
        self.cfg_file = os.path.join(self.cur_path, r"../Getera-93/CONFIG.DRV")

        self.input_file = os.path.join(self.cur_path, r"../Getera-93/prakticeInput/", self.filename + ".dat")

        self.burn_n = burn_n
        self.burn_time = burn_time

        self.uo2_x1 = Fuel(0.15)
        self.uo2_x2 = Fuel(0.13)
        self.b4c_pel = B4C(0.7)
        self.b4c_az = B4C(0.9)
        self.h2o = H2O()
        self.gd2o3 = Gd2o3()

        self.shall = Shall(1.0)
        self.shall_b4c = Shall_B4C()

    def prepare_data(self):
        self.uo2_x1.calc_nuclear_density()
        self.uo2_x2.calc_nuclear_density()
        self.b4c_pel.calc_nuclear_density()
        self.b4c_az.calc_nuclear_density()
        self.shall.calc_nuclear_density()
        self.h2o.calc_nuclear_density()
        self.shall_b4c.calc_nuclear_density()
        self.gd2o3.calc_nuclear_density()

        data = {}
        data["tv_1"] = {"u235": self.uo2_x1.U235.N, "u238": self.uo2_x1.U238.N, "o": self.uo2_x1.O.N}
        data["tv_2"] = {"u235": self.uo2_x2.U235.N, "u238": self.uo2_x2.U238.N, "o": self.uo2_x2.O.N}
        data["Zr_ob_tv"] = {"zr": self.shall.Zr.N}
        data["H2O"] = {"h": self.h2o.H.N, "o": self.h2o.O.N}
        data["SVP"] = {"o": self.gd2o3.O.N, "gd55": self.gd2o3.Gd55.N, "gd57": self.gd2o3.Gd57.N}
        data["az"] = {"b-10": self.b4c_az.B10.N, "c": self.b4c_az.C.N}
        data["PEL"] = {"b-10": self.b4c_pel.B10.N, "c": self.b4c_pel.C.N}
        data["ob_pel"] = {"ni": self.shall_b4c.Ni.N, "cr": self.shall_b4c.Cr.N}

        return data

    def render(self):
        data = self.prepare_data()

        template = Template(filename=self.template_path)
        input_file = template.render(el=data, burn_n=self.burn_n, burn_time=self.burn_time)
        input_file = input_file.replace("\r\n", "\n")
        input_file = input_file.replace("\n\n", "\n")

        cfg_template = Template(filename=self.cfg_template_path)
        cfg_file = cfg_template.render(input_file=self.filename)
        cfg_file = cfg_file.replace("\r\n", "\n")
        cfg_file = cfg_file.replace("\n\n", "\n")

        with open(self.cfg_file, 'w') as file:
            file.write(cfg_file)

        with open(self.input_file, 'w') as file:
            file.write(input_file)

    def start(self):
        self.render()

        getera_folder, getera_exe = os.path.split(self.getera_exe_path)
        cur_dir = os.getcwd()
        os.chdir(getera_folder)
        os.system(getera_exe)
        # time.sleep(2)
        os.chdir(cur_dir)

if __name__ == "__main__":
    template_names = os.listdir("../templates")

    for template_name in template_names:
        getera = Getera(template_name)
        getera.start()

