class CreateBrands < ActiveRecord::Migration
  def change
    create_table :brands do |t|
    	t.string :title
    	t.string :subtitle
    end
  end
end
