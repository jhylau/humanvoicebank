class CreatePerks < ActiveRecord::Migration
  def change
    create_table :perks do |t|
    	t.integer :category_id
    	t.integer :brand_id
    	t.string :title
    	t.string :subtitle
    	t.text :description
    end
  end
end
