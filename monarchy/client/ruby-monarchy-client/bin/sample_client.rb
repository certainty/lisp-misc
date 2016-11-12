#!/usr/bin/env ruby

$:.unshift(__dir__ + '/../lib')
require 'ruby-monarchy-client'


if ARGV.empty?
  puts "Usage: #{$0} api-uri"
  exit 1
end


config = {
  uuid: "3f013bb0-95ae-4396-8c06-3c622bea8cf4",
  uri: ARGV[0]
}



client = Monarchy::Client.new(config)

# send data from a simple heartbeat actor
begin
  puts "Pushing sample to #{config[:uri]}"
  puts client.push_sample({ heartbeat: { time: Time.now.utc.to_i } }).inspect

  exit(0)
rescue => ex
  puts ex.inspect
  exit(1)
end
