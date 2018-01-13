class CreateNotepads < ActiveRecord::Migration[5.1]
  def change
    create_table :notepads do |t|
      t.string :creator_token
      t.string :title
      t.text :description
      t.jsonb :content

      t.timestamps
    end
  end
end
