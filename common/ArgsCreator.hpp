#pragma once


#ifndef RADIUM_COMMON_ARGSCREATOR_HPP
#define RADIUM_COMMON_ARGSCREATOR_HPP

#include <QString>
#include <QStringList>
#include <QCoreApplication>

namespace radium
{

class ArgsCreator
{
	QStringList args;

	int argc;
	const char **argv;

	bool is_dirty;

	void free_argv()
	{
		for(int i=0;i<argc;i++)
			free((void*)argv[i]);
		free((void*)argv);
	}

	void create()
	{
		free_argv();
		argc = args.size();
		argv = (const char**)calloc(argc, sizeof(char*));
		for(int i=0;i<argc;i++)
			argv[i] = strdup(args[i].toUtf8().constData());
		is_dirty = false;
	}

public:
	
	void push_back(QString arg)
	{
		args.push_back(arg.replace("%radium_path%",QCoreApplication::applicationDirPath()));
		is_dirty = true;
	}
	
	void push_back(QStringList args2)
	{
		for(auto arg : args2)
			push_back(arg);
	}

	int get_argc(void)
	{
		return args.size();
	}

	const char** get_argv(void)
	{
		if (is_dirty)
			create();
		return argv;
	}

	ArgsCreator()
		: argc(0)
		, argv(NULL)
		, is_dirty(true)
	{}

	~ArgsCreator(){
		free_argv();
	}
};

} // radium namespace

#endif
