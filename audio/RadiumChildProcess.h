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

// A small child-process wrapper.
//
// On POSIX, juce::ChildProcess launches processes with fork() + execvp() and
// does non-async-signal-safe work (String conversions, memory allocation)
// between the two. In a heavily multi-threaded process like Radium, the child
// can deadlock before exec if another thread happens to hold the malloc or Qt
// lock when fork() is called. posix_spawn() does all of that work in the
// parent before the child is created, so this wrapper uses posix_spawn() on
// POSIX. Windows keeps using juce::ChildProcess.
//
// The wrapper also always reaps the child. kill() kills the process and waits
// for it to be reaped, and the destructor does the same if a process is still
// running, so cancelling a scan no longer leaves a zombie behind.
//
// Include this after JuceHeader.h.

#if !JUCE_WINDOWS

#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <chrono>
#include <string>
#include <thread>
#include <vector>

extern char **environ;

#endif

namespace radium{

class ChildProcess {
public:

  ChildProcess() {}

  ChildProcess(const ChildProcess&) = delete;
  ChildProcess& operator=(const ChildProcess&) = delete;
  ChildProcess(ChildProcess&&) = delete;
  ChildProcess& operator=(ChildProcess&&) = delete;

  ~ChildProcess(){
    kill();
  }

  bool start(const juce::StringArray &arguments){
#if JUCE_WINDOWS
    if (process.isRunning())
      return false;

    // Don't connect stdout/stderr to the pipe created by juce::ChildProcess;
    // the pipe is never read, and a chatty child could fill it and block.
    return process.start(arguments, 0);
#else
    // If a previous child is still running, don't overwrite its pid.
    if (!reap_if_finished())
      return false;

    if (arguments.isEmpty())
      return false;

    // Build argv before spawning. The child is created by posix_spawnp()
    // below, after all allocations and String conversions are done.
    std::vector<std::string> arg_storage;
    arg_storage.reserve(arguments.size());

    for (const auto &argument : arguments){
      if (argument.isNotEmpty())
        arg_storage.push_back(argument.toStdString());
    }

    if (arg_storage.empty())
      return false;

    std::vector<char*> argv;
    argv.reserve(arg_storage.size() + 1);

    for (auto &argument : arg_storage)
      argv.push_back(const_cast<char*>(argument.c_str()));

    argv.push_back(NULL);

    posix_spawn_file_actions_t file_actions;

    if (posix_spawn_file_actions_init(&file_actions) != 0)
      return false;

    // juce::ChildProcess captures the output in a pipe that is never read.
    // Redirecting to /dev/null instead also avoids blocking the child if it
    // writes more than a pipe can hold.
    int file_actions_result = posix_spawn_file_actions_addopen(
        &file_actions, STDOUT_FILENO, "/dev/null", O_WRONLY, 0);

    if (file_actions_result == 0)
      file_actions_result = posix_spawn_file_actions_addopen(
          &file_actions, STDERR_FILENO, "/dev/null", O_WRONLY, 0);

    if (file_actions_result != 0){
      posix_spawn_file_actions_destroy(&file_actions);
      return false;
    }

    pid_t pid = 0;
    const int result = posix_spawnp(&pid, argv[0], &file_actions, NULL, argv.data(), environ);

    posix_spawn_file_actions_destroy(&file_actions);

    if (result != 0)
      return false;

    pid_ = pid;
    return true;
#endif
  }

  bool waitForProcessToFinish(int timeout_ms){
#if JUCE_WINDOWS
    return process.waitForProcessToFinish(timeout_ms);
#else
    const auto start_time = std::chrono::steady_clock::now();

    for(;;){
      if (reap_if_finished())
        return true;

      if (timeout_ms >= 0){
        const auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - start_time).count();
        if (elapsed >= timeout_ms)
          return false;
      }

      std::this_thread::sleep_for(std::chrono::milliseconds(2));
    }
#endif
  }

  void kill(void){
#if JUCE_WINDOWS
    if (process.isRunning()){
      process.kill();
      process.waitForProcessToFinish(-1);
    }
#else
    if (pid_ > 0){
      ::kill(pid_, SIGKILL);
      reap_blocking();
    }
#endif
  }

private:

#if JUCE_WINDOWS

  juce::ChildProcess process;

#else

  // Blocks until the child has exited and has been reaped.
  void reap_blocking(void){
    while (pid_ > 0){
      int status = 0;
      pid_t result;

      do {
        result = waitpid(pid_, &status, 0);
      } while (result < 0 && errno == EINTR);

      if (result == pid_ || (result < 0 && errno == ECHILD)){
        pid_ = -1;
        return;
      }

      // Unexpected waitpid error. Leave pid_ set rather than loop forever.
      return;
    }
  }

  // Returns true if the child has exited (and has been reaped).
  bool reap_if_finished(void){
    if (pid_ <= 0)
      return true;

    int status = 0;
    pid_t result;

    do {
      result = waitpid(pid_, &status, WNOHANG);
    } while (result < 0 && errno == EINTR);

    if (result == pid_ || (result < 0 && errno == ECHILD)){
      pid_ = -1;
      return true;
    }

    return false;
  }

  pid_t pid_ = -1;

#endif
};

} // namespace radium
