class CreateSentences < ActiveRecord::Migration
  def change
    create_table :sentences do |t|
      t.string :sentence
  	  t.string :phonetic
  	  t.string :flag
  	  t.string :filename
    end
  end
end
