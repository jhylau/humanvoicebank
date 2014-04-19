require 'csv'
desc "Imports a CSV file into an ActiveRecord table"
task :import => :environment do    
	file = "db/sentences.csv"
    CSV.foreach(file, :headers => true) do |row|
      Sentence.create!( 
      :filename => row[0],
      :sentence => row[1],
      :phonetic => row[2],
      :flag => row[3]
      )
    end
end