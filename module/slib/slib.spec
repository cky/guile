%define name slib
%define version 2d1
%define release 1

Name:         %{name}
Release:      %{release}
Version:      %{version}
Packager:     Radey Shouman <shouman@ne.mediaone.net>

Copyright:    distributable, see individual files for copyright
Vendor:       Aubrey Jaffer <jaffer@ai.mit.edu>
Group:        Development/Tools
Provides:     slib
BuildArch:    noarch

Summary: platform independent library for scheme
Source:       ftp://swissnet.ai.mit.edu/pub/scm/slib%{version}.zip
URL:          http://swissnet.ai.mit.edu/~jaffer/SLIB.html
BuildRoot:    %{_tmppath}/%{name}%{version}
Prefix:       /usr/share

%description
"SLIB" is a portable library for the programming language Scheme.
It provides a platform independent framework for using "packages" of
Scheme procedures and syntax.  As distributed, SLIB contains useful
packages for all Scheme implementations.  Its catalog can be
transparently extended to accomodate packages specific to a site,
implementation, user, or directory.

%define __os_install_post /usr/lib/rpm/brp-compress

%prep
%setup -n slib -c -T
cd ..
unzip $RPM_SOURCE_DIR/slib%{version}.zip

%build
gzip -f slib.info

%install
mkdir -p ${RPM_BUILD_ROOT}%{prefix}/slib
cp -r . ${RPM_BUILD_ROOT}%{prefix}/slib
mkdir -p ${RPM_BUILD_ROOT}/usr/info
cp slib.info.gz ${RPM_BUILD_ROOT}/usr/info

%clean
rm -rf $RPM_BUILD_ROOT

%post
/sbin/install-info /usr/info/slib.info.gz /usr/info/dir

# This symlink is made as in the spec file of Robert J. Meier.
if [ -L /usr/share/guile/slib ]; then
  rm /usr/share/guile/slib
  ln -s %{prefix}/slib /usr/share/guile/slib
fi

# This section should be extended to rebuild catalogs for as many
# implementations as possible.
if [ -x /usr/bin/guile ]; then
  /usr/bin/guile -c "(use-modules (ice-9 slib)) (require 'new-catalog)"
fi
if [ -x /usr/bin/scm ]; then
  /usr/bin/scm -c "(require 'new-catalog)" 
fi

%files
%defattr(-, root, root)
%dir %{prefix}/slib
%{prefix}/slib/*.scm
%{prefix}/slib/*.init
/usr/info/slib.info.gz
# The Makefile is included as it is useful for building documentation.
%{prefix}/slib/Makefile
%doc ANNOUNCE ChangeLog FAQ README

%changelog
* Wed Mar 14 2001 Radey Shouman <shouman@ne.mediaone.net>
- Adapted from the spec file of R. J. Meier.

* Mon Jul 12 2000 Dr. Robert J. Meier <robert.meier@computer.org> 0.9.4-1suse
- Packaged for SuSE 6.3

* Sun May 30 2000 Aubrey Jaffer <jaffer@ai.mit.edu>
- Updated content
