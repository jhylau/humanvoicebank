class Perk < ActiveRecord::Base
  extend FriendlyId
  friendly_id :title, use: :slugged

  # belongs_to :brand
  # belongs_to :category
  
  mount_uploader :image, ImageUploader

  # validates_presence_of :category_id, :subtitle, :title

  include PgSearch
  # pg_search_scope :perk_search, :against => [:title, :subtitle, :description, :website, :city, :state], :associated_against => {
  #   :category => [:title, :subtitle]
  # }

  def self.text_search(query)
    if query.present?
      search(query)
    else
      scoped
    end
  end
end
