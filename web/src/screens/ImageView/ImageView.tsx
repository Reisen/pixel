// This Screen renders an actual image view, it shows the image in question
// along with all metadata, tags, and operations that can be performed on it.
// Note that this screen isn't responsible for rendering an image clicked on
// from a users public stream. Only gallery views.

import React, { useEffect }       from 'react';
import styles                     from './ImageView.module.css';
import { State }                  from '../../store';
import { connect }                from 'react-redux';
import { match }                  from 'react-router-dom';
import { getImages, fetchImages } from '../../store/images';
import { uploadTags, deleteTags } from '../../api/images';
import { Image as image }         from '../../api/types';

// Components
import ImagePanel                 from './components/ImagePanel';
import MetaDataPanel              from '../../panels/MetaDataPanel';
import NavigationBar              from '../../components/NavigationBar';
import SearchSidebar              from '../../components/SearchSideBar';
import SettingsPanel              from '../../panels/SettingsPanel';
import TagPanel                   from '../../panels/TagPanel';


interface Params {
    uuid?: string;
}

interface Props {
    image?:      image;
    username:    string;
    images:      image[];
    fetchImages: () => void;
    match:       match<Params>;
}

const headerLinks = [
    {name: 'Images', path: ''},
    {name: 'Pools', path: ''},
    {name: 'Tags', path: ''}
];

const Image = (props: Props) => {
    const { fetchImages } = props
    useEffect(() => { fetchImages() }, [fetchImages]);

    // Scan for image by UUID, and extract the tags into a counted tag list.
    const image = props.images.find(image => image.UUID === props.match.params.uuid);
    const tags  = image && image.tags.map((tag: string): [string, number] => [tag, 1]);

    // Update Tags for an Image
    const handleTagsUpdate = async (adds: string[], deletes: string[]) => {
        if (!image) { return; }
        await uploadTags(image.UUID, { tags: adds });
        await deleteTags(image.UUID, { tags: deletes });
        await props.fetchImages();
    };

    return !image
        ? <span>Ruh oh</span>
        : (
        <div className="Page">
            <NavigationBar links={headerLinks} username={props.username} />
            <div className={styles.Root}>
                <SearchSidebar initialPanel="Tag List">
                    {[
                        {
                            icon: 'tag',
                            name: 'Tag List',
                            elem: <TagPanel
                                editable
                                onSave={handleTagsUpdate}
                                uuid={image.UUID}
                                tags={tags}
                            />
                        },
                        {
                            icon: 'gears',
                            name: 'Settings',
                            elem: <SettingsPanel />
                        },
                        {
                            icon: 'chart-radar-graph',
                            name: 'Metadata',
                            elem: <MetaDataPanel />
                        }
                    ]}
                </SearchSidebar>

                <ImagePanel image={image} />
            </div>
        </div>
    );
}

const mapState = (state: State) => ({
    images:   getImages(state.images),
    username: state.user.username
});

const mapDispatch = {
    fetchImages
};


export default connect(mapState, mapDispatch)(Image);
