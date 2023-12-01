
from jinja2 import Template

# Your data
data = {
        'hostname': "KUDU",
        'timezone': "europe/Vatican",
        'locale': "sv_SE.UTF-8",
        'disk_bootloader': "/dev/sda1",
}

# Read the template from the file
with open('template.scm', 'r') as file:
    template_content = file.read()

# Create a Jinja2 template object
template = Template(template_content)

# Render the template with your data
result = template.render(data)

# Print the rendered result
print(result)
