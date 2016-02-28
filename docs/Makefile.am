## Process this file with automake to produce Makefile.in

.PHONY: docs
docs:
	$(top_builddir)/docs/texi2html --split nodes a-sync.texi
	cp a-sync/a-sync.html a-sync/index.html
	rm -rf html
	mv a-sync html

doc_DATA = example.scm

install-data-hook:
	install -d $(DESTDIR)$(htmldir)/html
	install -m a+r,u+w html/* $(DESTDIR)$(htmldir)/html/

uninstall-hook:
	for files in `ls html`; do rm -f $(DESTDIR)$(htmldir)/html/$$files; done

EXTRA_DIST = html/*.html *.texi texi2html
