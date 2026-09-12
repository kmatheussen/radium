/* Copyright 2026 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */

#pragma once

#include <QString>
#include <QStringList>

QStringList FAUST2_lint_faust_code(const QString &code,
                                   bool compile_check_safe,
                                   const QString &radium_path = QString());

QString FAUST2_splice_faust_definitions(const QString &current_code, const QString &fragment);

bool FAUST2_try_fix_duplicate_definition(const QString &code,
                                         const QString &name,
                                         QString *fixed_code,
                                         QString *note);

bool FAUST2_try_fix_smoothed_delay_param(const QString &code,
                                         QString *fixed_code,
                                         QString *note);
