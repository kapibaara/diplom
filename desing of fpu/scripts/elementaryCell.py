import os
from getera import Getera
from mako.template import Template
from getera_graphics import read_file, get_burn_time, plot_spline, get_Kinf
from sketch import write_file

if __name__ == "__main__":
    cur_path = os.path.split(__file__)[0]
    template_name = r"elementaryCell.mako"
    getera_out_path = os.path.join(cur_path, r"../Getera-93/prakticeOutput/")
    getera_output_file = os.path.join(cur_path, getera_out_path, r"elementaryCell.out")

    d2o_concentrations = [0, 0.2, 0.4, 0.6, 0.8, 1]
    reactivity_margin = []

    for d2o_concentration in d2o_concentrations:
        getera = Getera(template_name, 0, d2o_concentration)
        getera.start()

        data = read_file(getera_output_file)
        kinf = get_Kinf(data)

        reactivity_margin.append(kinf[0])

    print(reactivity_margin)