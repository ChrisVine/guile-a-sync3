/*
  Copyright Chris Vine 2016

  This library is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 3 of the
  License, or (at your option) any later version.
 
  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
 
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
  02110-1301 USA
*/

#include <unistd.h>
#include <time.h>
#include <errno.h>

#include <libguile.h>

#include <config.h>


static SCM have_monotonic_time(void) {
#ifdef HAVE_MONOTONIC_CLOCK
  return SCM_BOOL_T;
#else
  return SCM_BOOL_F;
#endif
}

/* returns a (sec . usec) pair.  It throws an 'a-sync-exception guile
   exception if the library has been configured for monotonic time at
   configuration time but it is not in fact supported, but this is not
   worth testing for by user code as it should never happen - the
   library configuration macros should always give the correct
   answer */
static SCM get_time(void) {
#ifdef HAVE_MONOTONIC_CLOCK
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts)) {
    scm_throw(scm_from_latin1_symbol("a-sync-exception"),
	      scm_list_4(scm_from_latin1_string("get-time"),
	      		 scm_from_latin1_string("guile-a-sync: ~A"),
	      		 scm_list_1(scm_from_latin1_string("monotonic time not supported "
							   "by underlying implementation")),
	      		 scm_from_int(errno)));
  }
  return scm_cons(scm_from_size_t(ts.tv_sec), scm_from_long(ts.tv_nsec/1000L));
#else
  return scm_gettimeofday();
#endif
}

void init_a_sync_monotonic_time(void* unused) {
  scm_c_define_gsubr("have-monotonic-time", 0, 0, 0, have_monotonic_time);
  scm_c_define_gsubr("get-time", 0, 0, 0, get_time);
  scm_c_export("have-monotonic-time", NULL);
  scm_c_export("get-time", NULL);
}
