## Process this file with automake to produce Makefile.in

.PHONY: docs
docs:
	makeinfo guile-a-sync3.texi
	makeinfo --html -c FORMAT_MENU=menu guile-a-sync3.texi
	rm -rf html
	mv guile-a-sync3 html

doc_DATA = example.scm example-glib.scm example-g-golf.scm \
	   example-guile-gi.scm example-client.scm example-server.scm

infofiledir = $(infodir)
infofile_DATA = guile-a-sync3.info

install-data-hook:
	install -d $(DESTDIR)$(htmldir)/html
	install -m a+r,u+w html/* $(DESTDIR)$(htmldir)/html/

uninstall-hook:
	for files in `ls html`; do rm -f $(DESTDIR)$(htmldir)/html/$$files; done

EXTRA_DIST = html/*.html *.texi guile-a-sync3.info $(doc_DATA)
