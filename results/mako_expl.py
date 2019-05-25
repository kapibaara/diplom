from mako.template import Template

mytemplate = Template("${name} - гвупая соба")
print(mytemplate.render(name='Пафа'))
