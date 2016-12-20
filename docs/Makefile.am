## Process this file with automake to produce Makefile.in

.PHONY: docs
docs:
	makeinfo guile-a-sync2.texi
	makeinfo --html guile-a-sync2.texi
	rm -rf html
	mv guile-a-sync2 html

doc_DATA = example.scm example-glib.scm example-client.scm example-server.scm

infofiledir = $(infodir)
infofile_DATA = guile-a-sync2.info

install-data-hook:
	install -d $(DESTDIR)$(htmldir)/html
	install -m a+r,u+w html/* $(DESTDIR)$(htmldir)/html/

uninstall-hook:
	for files in `ls html`; do rm -f $(DESTDIR)$(htmldir)/html/$$files; done

EXTRA_DIST = html/*.html *.texi guile-a-sync2.info $(doc_DATA)
