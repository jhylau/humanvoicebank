class Brand < ActiveRecord::Base
  extend FriendlyId
  friendly_id :title, use: :slugged
  
  has_many :perks

  mount_uploader :image, ImageUploader

  # validates_presence_of :category_id, :subtitle, :title

  include PgSearch
  # pg_search_scope :perk_search, :against => [:title, :subtitle, :description, :website, :city, :state], :associated_against => {
  #   :category => [:title, :subtitle]
  # }


end
