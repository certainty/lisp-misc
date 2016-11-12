load File.dirname(__FILE__) + '/lib/configuration.rb'
load File.dirname(__FILE__) + '/lib/client.rb'

if $0 == __FILE__
  puts "executing self"
  sample = { :time => Time.now.to_i, :message => "ruby-hello monarchy" }
  puts = Monarchy::Client::Client.new.post(sample)
end

