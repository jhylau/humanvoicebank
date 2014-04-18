class CreateRecordings < ActiveRecord::Migration
  def change
    create_table :recordings do |t|
      t.integer :user_id
      t.integer :sentence_id
      t.string :audio
    end
  end
end
