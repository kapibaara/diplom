from mako.template import Template
import os

exmple_path = os.path.split(__file__)[0]
template_path = os.path.join(exmple_path, r"..\templates\pr_AZ_NotAZ.mako")
template = Template(filename=template_path)
print(template.render())