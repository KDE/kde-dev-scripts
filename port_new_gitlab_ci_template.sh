#!/bin/sh
# Copyright 2023 by Laurent Montel <montel@kde.org> licensed under GPL.
# 

if grep -q "project: sysadmin/ci-utilities" .gitlab-ci.yml; then
    echo "File already ported"
else

    perl -pi -e 's,include:,include:\n  - project: sysadmin/ci-utilities\n    file:,' .gitlab-ci.yml

    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/linux-qt6.yml,    - /gitlab-templates/linux-qt6.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/freebsd-qt6.yml,    - /gitlab-templates/freebsd-qt6.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/windows-qt6.yml,    - /gitlab-templates/windows-qt6.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/reuse-lint.yml,    - /gitlab-templates/reuse-lint.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/android-qt6.yml,    - /gitlab-templates/android-qt6.yml,' .gitlab-ci.yml

    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/linux.yml,    - /gitlab-templates/linux.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/freebsd.yml,    - /gitlab-templates/freebsd.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/windows.yml,    - /gitlab-templates/windows.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/android.yml,    - /gitlab-templates/android.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/flatpak.yml,    - /gitlab-templates/flatpak.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/craft-appimage.yml,    - /gitlab-templates/craft-appimage.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/windows-static.yml,    - /gitlab-templates/windows-static.yml,' .gitlab-ci.yml
    perl -pi -e 's,- https://invent.kde.org/sysadmin/ci-utilities/raw/master/gitlab-templates/linux-static.yml,    - /gitlab-templates/linux-static.yml,' .gitlab-ci.yml

fi
