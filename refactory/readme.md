Dependency based programming for CHICKEN scheme
================================================

Sake is a system to do dependency based programming in scheme. It provides
primitives to create tasks that might have dependencies and provides a runtime
that resolves the dependencies and invokes them in order.

Examples:

    (define-task test (no-deps)
     "I'm a simple task without dependencies"
     (lambda () (print "test called")))

    (define-task test2 (depends-on test)
      (lambda () (print "test2 called. I have no documentation")))

    (run-task test2)

    ;; test called
    ;; test2 called. I have no documentation


Note that the above examples make it really easy to define tasks.
You can however always use the procedural interface as well.

    (define t (make-task 'test (lambda () (print "test")) (no-deps) "test doc"))

This will create a task with no dependencies that just prints "test".

Note that (no-deps) and (depends-on) are just higher order procedures that
can be used to create dependency-procedures. The dependencies of a task
are expressed by a procedure that is supposed to return the list of tasks
that the given task shall depend on. Using no-deps or depends-on is completely
optional. You could implement your own procedures that assemble the list
of dependency on runtime based on some constraints.

Finally there is a form that let's you conveniently inject new tasks (and thus
new dependencies) into the system (beside using the procedural interface)

    (task-lambda (no-deps) (print "I'm anonymous")))

This creates an anonymous (well a uniquely named rather) task, that is first
class just like any other task.
