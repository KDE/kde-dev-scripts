#!/usr/bin/env ruby

module DCOP

  def dump_all_apps

    `dcop`.split(/\n/).each do

      |app|

      DCOP.dump_app(app)

    end

  end

  def dump_app(app)

    print "<app name=\"#{app}\">\n"

    `dcop #{app}`.split(/\n/).each do

      |object|

      DCOP.dump_object(app, object)

    end

    print "</app>\n"

  end

  def dump_object(app, object)

    object.gsub!(/\(default\)/, '')
    object.strip!

    print "  <object name=\"#{object}\">\n" unless object == "(default)"

    `dcop #{app} #{object}`.split(/\n/).each do

      |method|

      DCOP.dump_method(app, object, method)

    end

    print "  </object>\n"

  end

  def dump_method(app, object, method)

    return_type, method_name, arg_str = method.split(/[ \(]/, 3)

    arg_str.gsub!(/\)$/, '')

    arg_list = arg_str.split(',')

    print "    <method name=\"#{method_name}\" return-type=\"#{return_type}\""

    if arg_list.empty?

      print "/>\n"
      return

    else

      print ">\n"

      arg_list.each do

        |arg|

        type, name = arg.split

        print "      <parameter name=\"#{name}\" type=\"#{type}\"/>\n"

      end

      print "    </method>\n"

    end

  end

  module_function :dump_all_apps, :dump_app, :dump_object, :dump_method

end

DCOP.dump_all_apps if __FILE__ == $0
