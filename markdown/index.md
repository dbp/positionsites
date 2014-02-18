# Position Sites

--------

## Overview

Position Sites is fundamentally based on a single concept: _data_. In various CMS's we've used, we were constantly frustrated by being pidgeon-holed into preconceived notions of what types of data we would be using. Usually, the fundamental structure would be a post. We could add a few custom fields, or try to build structure via special tags or categories, but the data structures were limited.

Of course, you gain all that flexibility back if you skip using a CMS and just write a web application from scratch using your web framework of choice. But Position Sites is trying to sit in the middle of these two worlds - providing the ease and rapid construction of a CMS and the data level flexibility of a web framework. Just so you don't think we're insane, we _aren't_ trying to provide the other flexibility of a web framework. This is about content managament, not code.

## Data Definitions

Data types are descriptions of a type of data that will appear on the site. Examples could include products in a catalog, articles in a magazine, paintings in a gallery, etc. The fundamental property about them is that many can be created, and that each one has a set of named attributes. The type of the attributes that are currently supported are:

- string - a field of unstructured text
- number - a numeric field
- image - an image
- list - a list of some type of field that is not itself a list
- data - a different data type

The current way that data types are defined is based on the JSON format. We represent a data type as:

    {fieldname: fieldtype, fieldname: fieldtype ...}

Where `fieldtype` is one of:

- `"string"`
- `"number"`
- `"image"`
- `[fieldtype]`
- `{"data": "name-of-other-data"}`

As a concrete example, one of our projects involves the following data types:

- `painting`:

        {"image":"image"
        ,"caption":"string"
        ,"title":"string"}

- `artist`:

        {"bio":"string"
        ,"name":"string"
        ,"featured":{"data":"painting"}
        ,"paintings":[{"data":"painting"}]}

This is the full definition that is needed to describe a structure where we have many artists, and each one has many paintings, with one chosen as featured.


## Html Templates

The other half of the design is HTML templates. The goal is that you can define some data types, and that at that point, all you need to do is write the html that will display the content, including links that will allow people to add, edit, and remove content. The mechanism by which this works are custom html tags that are generated based on the data definitions that you've given.

Brief side trip:

> For all tags mentioned, if they render text (not other tags), you can also use them inside html attributes, but they take the form of `${tag-name}` instead of `<tag-name/>`. This form cannot take arguments, so if you need to use a tag with arguments you can give it a name with:
>
>     <bindStrict tag="some-name">
>     ... complicated content
>     </bindStrict>
>
> After which you can use `${some-name}` in attributes. For example, `<a href="/artist/${id}>`. There is one other utility provided: renaming a tag. This is helpful because many share the same name - if you want the `<id/>` tag for the artist, but you are inside the context of a painting of theirs, then `<id/>` will be the painting's id, not the artist. In this case, you can rename the tag to something else, like:
>
>     <rebind old="id" new="artist-id"/>
>
> After which, `<artist-id/>` and `${artist-id}` will produce the value that `<id/>` did where the `rebind` tag occurs.


End side trip.

Assuming you defined data types as above, the four main new html tags you would have access to in your templates are:

    <new-painting/>
    <all-painting></all-painting>
    <new-artist/>
    <all-artist></all-artist>

The two `new-type` tags are just buttons that have a `+` on them, and when you click on them, a box pops up that allows you to fill in the fields of the data type, and then save the new item. If the person viewing the page that has that tag is not logged in, nothing shows up. We allow you to customize the form that pops up slightly, but intentionally littly - because each extra customization means you are spending more time building stuff we don't think you should be wasting your time on! What we do allow you to control is which fields show up, and in what order. Since there (currently) is no way to create paintings while you are creating an artist, including that field is pointless, as is the featured, as they don't have any paintings yet. So you would probably write the `new-artist` tag as:

    <new-artist order="name,bio"/>

The `all-type` tags are a way to display the items on the page. What it does is bind more special tags in it's body, and then runs the body for each item of that type. For example, if we wanted to list all of the artist's names, we could do this as:

    <ul>
      <all-artist>
        <li><name/></li>
      </all-artist>
    </ul>

As you might expect, the field names are bound to the value in the field. How they behave depends on what type of field they are:

- string: A tag that just is the value of the string
- number: Like strings, just shows the number
- image: Produces an `<img>` tag from the image. This takes an extra attribute, `size`, that is either a single number "N", or "NxM", where "N" is shorthand for "NxN". This is the maximum size of the image, and the tag scales down the image accordingly (it will make multiple sizes, and choose the most efficient, in terms of bandwidth, version to use).
- data: This will run it's body with the fields to the other data type bound. For example:

        <ul>
          <all-artist>
            <li><featured><title/></featured></li>
          </all-artist>
        </ul>

    Would show the titles of each artist's featured painting. It also binds a special tags `<exists>` that only runs its contents if there is actually a data element there (as there could be none assigned).
- list: This runs it's body with a couple tags bound, the most important of which is `<element>` - which behaves like the type of field that is in the list (so if it is a list of strings, it will just show the field, whereas if it is a list of data, it will run it's children with fields bound). For example:

        <ul>
          <all-artist>
            <li><paintings><element><title/>, </element></paintings></li>
          </all-artist>
        </ul>

    Would show a comma-separated list of the titles of all paintings for artist.

These are the basics, but there are some more tags that are bound, to be able to control things more finely, and also be be able to edit items.

Within the context of each item (so inside an `all-data` tag, or an `element` tag, or a `field` tag where `field` is a data type) there is a `<delete/>` tag. This creates a button, only visible when the user has permission to delete it (we'll visit permissions later) that will pop up a confirmation window to delete the item. When deleting items from a list of data, the original items are not deleted, just the reference in the list. For example, we could add a link to delete the artist to our name example:

    <ul>
      <all-artist>
        <li><name/> <delete/></li>
      </all-artist>
    </ul>

There are also a variety of tags to set values. The simplest are the `set-field` tags, which appear in the same context as `delete`, and allow you to update the field. These work for simple fields, like `string`, `number`, and `image`, but there are more complicated ones for `data` and `list`, because there are more possibilities for what to do. As an example, we could allow people to edit their names in our example:

    <ul>
      <all-artist>
        <li><name/> <set-name/></li>
      </all-artist>
    </ul>

This will pop up a window with a form to change the name.

For `data` fields, there are two possibilies: we may want to create a new item and put it in the field, or we may want to select from existing items. Correspondingly, we have two tags: `set-existing` and `set-new`. For example, we could provide both options when choosing a featured painting:

    <ul>
      <all-artist>
        <li><featured><set-new/><set-existing/></featured></li>
      </all-artist>
    </ul>

For `set-existing`, we allow you to specify where the list of existing items should come from (by default it is all items you could edit; see permissions below). This is provided by the `from` attribute, which takes the form `datatype:ID.field`. For example:

    <set-existing from="artist:${id}.paintings"/>

For `list` fields, we get extra tags bound along side for adding new elements. If what is in the list is a primitive element, `add-field` works, and if it is a list of data then `add-field-new` and `add-field-existing` work similarly to `set-new` and `set-existing`, but adding to the list. Lists also get a final extra feature, which is `next` and `prev` tags, which produce buttons that move items around the list.

The last thing that all items get bound is an `id` tag. This is an implicit field on all items, and is the unique identifier for the item, which is used for pages that are specific to single items.


## Permissions

All items have an owner. The users are created through the backend, like the html pages and data definitions. Every item has an `<ownership>` tag which produces a button with it's contents as the text. It also has an `<owner>` tag that renders the current owner's username. For example, to change the owner of the paintings, we could produce:

    <ul>
      <all-painting>
        <li><name/> <ownership>current owner: <owner/></ownership></li>
      </all-painting>
    </ul>

And clicking on the "current owner: jane" link would allow you to pick from all users on the site. These links only show up for administrators, which are a certain type of user, and further, all users but administrators can only edit or delete items that they own. Administrators can edit or delete anything.

This is a very simplistic permissions system, but that should be a theme you've noticed - we are trying to make things simple, because if you want something more flexible, you should probably be writing a custom application!


## Pages

TODO
