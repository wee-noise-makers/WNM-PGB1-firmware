# Configuration file for the Sphinx documentation builder.
# sphinx/conf.py — confdir is sphinx/, sourcedir is build/sphinx/source/

project = 'PGB-1 User Manual'
author = 'Wee Noise Makers'
copyright = '2025, Wee Noise Makers'
language = 'en'

# Firmware version — update this for each release
release = '1.2.0'
branch = 'main'
version = release

_pdf_basename = 'pgb1-user-manual'
html_context = {
    'pdf_filename': f'{_pdf_basename}-{release}.pdf' if release else f'{_pdf_basename}.pdf',
}

extensions = [
    'myst_parser',
    'sphinx.ext.imgconverter',
]

myst_enable_extensions = [
    'colon_fence',
    'deflist',
    'tasklist',
    'attrs_block',
]

myst_heading_anchors = 3

source_suffix = {'.md': 'markdown'}

master_doc = 'index'

# -- HTML output -------------------------------------------------------------

html_theme = 'furo'
html_copy_source = False
html_theme_options = {
    "light_logo": "wnm_logo.svg",
    "dark_logo": "wnm_logo_white.svg",
    "light_css_variables": {
        "color-brand-primary": "#2e7d32",
        "color-brand-content": "#2e7d32",
    },
    "dark_css_variables": {
        "color-brand-primary": "#2e7d32",
        "color-brand-content": "#2e7d32",
    },
    "sidebar_hide_name": False,
    "navigation_with_keys": True,
    "top_of_page_buttons": ["edit"],
    "source_repository": "https://github.com/wee-noise-makers/WNM-PGB1-firmware",
    "source_branch": branch,
    "source_directory": "docs/user-manual/source",
}

# _static is relative to confdir (sphinx/)
html_static_path = ['_static']
html_css_files = ['custom.css']
html_title = 'PGB-1 User Manual'
templates_path = ['_templates']

html_sidebars = {
    "**": [
        "sidebar/brand.html",
        "sidebar/search.html",
        "sidebar/download.html",
        "sidebar/scroll-start.html",
        "sidebar/navigation.html",
        "sidebar/scroll-end.html",
    ]
}

# -- PDF output (LaTeX) ------------------------------------------------------

latex_documents = [
    ('index', 'pgb1-user-manual.tex', 'PGB-1 User Manual', 'Wee Noise Makers', 'manual'),
]
