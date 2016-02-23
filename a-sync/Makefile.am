## Process this file with automake to produce Makefile.in

install-data-hook:
	install -d $(DESTDIR)$(GUILE_SITE)/a-sync
	install -m a+r,u+w *.scm $(DESTDIR)$(GUILE_SITE)/a-sync/

uninstall-hook:
	for files in `ls *.scm`; do rm -f $(DESTDIR)$(GUILE_SITE)/a-sync/$$files; done
	-rmdir $(DESTDIR)$(GUILE_SITE)/a-sync

EXTRA_DIST = *.scm
