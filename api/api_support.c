/* Copyright 2001 Kjetil S. Matheussen

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


#include "../common/nsmtracker.h"

#include "api_support_proc.h"


/* Returns an array of PyObjects from a PyList object. */

PyObject **PYR_getPYOArray(int *numargs,PyObject *pylist){
	int lokke;
	PyObject **ret=NULL;

	if (PyList_Check(pylist)) {
		*numargs = PyList_Size(pylist);
		ret = talloc_atomic((*numargs)*sizeof(PyObject *));
		for (lokke = 0; lokke < (*numargs); lokke++) {
			ret[lokke] = PyList_GetItem(pylist,lokke);
		}
	}else{
		*numargs=-1;
	}

	return ret;
}


/* Returns an array of integers from a PyList object. */

int *PYR_getIntArray(int *numargs,PyObject *pylist){
	int lokke;
	int *ret;

	PyObject **poa;

	poa=PYR_getPYOArray(numargs,pylist);
	if((*numargs)==-1){
		return NULL;
	}

	ret = talloc_atomic((*numargs)*sizeof(int));
	for (lokke = 0; lokke < (*numargs); lokke++) {
		PyObject *o = poa[lokke];
		if (PyInt_Check(o)){
				ret[lokke] = PyInt_AsLong(o);
		}else{
			(*numargs)=-1;
			return NULL;
		}
	}

	return ret;
}


/* Returns an array of C objects from a PyList object
	containing instance-typed objects (from classes).
   The C object format is spesified from 'numattrs',
	'attrformat' and 'attrnames'. 'attrformat' is an array
   of integers where 0 is integer and 1 is float.
   'attrnames' is an array of the names of the attributes
   from the python objects.

	This is a bit hackish function, but very practical.
   (Not garantiued to work on unusual hardware/os/compilers)
*/


struct intstruct{
	int a;
};

struct floatstruct{
	float a;
};

void *PYR_getObjArray(
	int *numargs,
	PyObject *pylist,
	int numattrs,
	const int *attrformat,
	char **attrnames
){
	int lokke,lokke2;
	int num_ints=0;
	int num_floats=0;
	size_t objsize;
	char *ret;
	char *set;

	PyObject **poa;

	poa=PYR_getPYOArray(numargs,pylist);
	if((*numargs)==-1){
		return NULL;
	}

	for(lokke=0;lokke<numattrs;lokke++){
		if(attrformat[lokke]==0){
			num_ints++;
		}else{
			num_floats++;
		}
	}

	objsize=sizeof(int)*num_ints+(sizeof(float)*num_floats);


	set=ret=talloc_atomic(objsize*(*numargs));

	for(lokke=0;lokke<(*numargs);lokke++){
		struct intstruct *is;
		struct floatstruct *fs;
		PyObject *po=poa[lokke];

		for(lokke2=0;lokke2<numattrs;lokke2++){
			PyObject *o = PyObject_GetAttrString(po,attrnames[lokke2]);
			if(o==NULL){
				(*numargs)=-1;
				return NULL;
			}

			if(attrformat[lokke2]==0){
				if (PyInt_Check(o)){
					is=(struct intstruct *)set;
					is->a = PyInt_AsLong(o);
				}else{
					(*numargs)=-1;
					return NULL;
				}
				set+=sizeof(int);
			}else{
				if (PyFloat_Check(o)){
					fs=(struct floatstruct *)set;
					fs->a = (float)PyFloat_AsDouble(o);
				}else{
					(*numargs)=-1;
					return NULL;
				}
				set+=sizeof(float);
			}
		}
	}
	return ret;
}












