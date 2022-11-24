#pragma once

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wundef"

#include <boost/version.hpp>
#if (BOOST_VERSION < 100000) || ((BOOST_VERSION / 100 % 1000) < 58)
  #error "Boost too old. Need at least 1.58.\n Quick fix: cd $HOME ; wget http://downloads.sourceforge.net/project/boost/boost/1.76.0/boost_1_76_0.tar.bz2 ; tar xvjf boost_1_76_0.tar.bz2 (that's it!)"
#endif

#ifndef RADIUM_NOT_INCLUDE_BOOST_QUEUE
#include <boost/lockfree/queue.hpp>
#endif

#ifdef RADIUM_INCLUDE_BOOST_STACK
#include <boost/lockfree/stack.hpp>
#endif

#pragma clang diagnostic pop


