/* Copyright 2012 Kjetil S. Matheussen

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

#ifndef VECTOR_PROC_H
#define VECTOR_PROC_H

extern LANGSPEC void VECTOR_push_back(vector_t *v, const void *element);
extern LANGSPEC void VECTOR_reverse(vector_t *v);
extern LANGSPEC vector_t *VECTOR_copy(vector_t *from);
extern LANGSPEC void VECTOR_clean(vector_t *v);
extern LANGSPEC vector_t *VECTOR_append(vector_t *v1, vector_t *v2);
extern LANGSPEC void VECTOR_delete(vector_t *v, int pos);
extern LANGSPEC int VECTOR_find_pos(vector_t *v, const void *element);
extern LANGSPEC bool VECTOR_is_in_vector(vector_t *v, const void *element);
extern LANGSPEC void VECTOR_remove(vector_t *v, const void *element);
extern LANGSPEC vector_t *VECTOR_list1_to_vector(struct ListHeader1 *list);
extern LANGSPEC vector_t *VECTOR_list3_to_vector(struct ListHeader3 *list);

#ifdef __cplusplus
#define VECTOR_FOR_EACH(type,varname,vector) {                              \
  int iterator666;                                                      \
  for(iterator666=0;iterator666<(vector)->num_elements;iterator666++){  \
  type varname = (type)((vector)->elements[iterator666]);
#else
#define VECTOR_FOR_EACH(var,vector) {                     \
  int iterator666;                                                      \
  for(iterator666=0;iterator666<(vector)->num_elements;iterator666++){ \
  var = (vector)->elements[iterator666];
#endif
#define END_VECTOR_FOR_EACH }}


#endif
