import os
import glob

def extract_web_content(fn):
	name = ""
	description = ""
	url =""
	with open(fn) as f:
		section1 = True
		afterPurpose = False
		afterPublish = False
		get_next_line = False
		name = ""
		description = ""
		url =""
		for line in f:
			#First section is title
			if(section1 == True):
				if(get_next_line == True):
					name = line
					section1= False
					get_next_line = False
				if "(* ::Section:: *)" in line:
					get_next_line = True
			#Text following purpose/scope is the description
			if "Purpose and Scope" in line:
				afterPurpose = True
			if(afterPurpose==True):
				if get_next_line == True:
					if "(* ::Subsection:: *)" not in line:
						description+=line
					else:
						afterPurpose = False
						get_next_line = False
				if "(* ::Text:: *)" in line:
					get_next_line = True
			#http after Publish[$NotebookPage] is the url for the page.
			if "Publish[$NotebookPage]" in line:
				afterPublish=True
			if afterPublish==True and "http" in line:
				url = line[5:-3]
				afterPublish=False
	return(name,description,url)

fl_out = open("webpage_out.txt","w")
fl_out.write("""<pre class="wp-block-preformatted"><strong>Pages</strong>""")
pages = glob.glob("pages/*.m")
for page in pages:
	name,description,url = extract_web_content(page)
	fl_out.write("""<pre class="wp-block-preformatted"><strong>{}</strong>""".format(name))
	fl_out.write("""<a href="{}">{}</a></pre>""".format(url,url))
	fl_out.write("{}".format(description))
	fl_out.write("<br><br>")

fl_out.write("""<pre class="wp-block-preformatted"><strong>Scripts</strong>""")
scripts = glob.glob("scripts/*.m")
for page in scripts:
	name,description,url = extract_web_content(page)
	fl_out.write("""<pre class="wp-block-preformatted"><strong>{}</strong>""".format(name))
	fl_out.write("""<a href="{}">{}</a></pre>""".format(url,url))
	fl_out.write("{}".format(description))
	fl_out.write("<br><br>")

fl_out.write("""<pre class="wp-block-preformatted"><strong>Scripts</strong>""")
functions = glob.glob("functions/*.m")
for page in functions:
	name,description,url = extract_web_content(page)
	fl_out.write("""<pre class="wp-block-preformatted"><strong>{}</strong>""".format(name))
	fl_out.write("""<a href="{}">{}</a></pre>""".format(url,url))
	fl_out.write("{}".format(description))
	fl_out.write("<br><br>")
fl_out.close()

