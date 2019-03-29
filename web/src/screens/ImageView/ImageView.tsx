import React, { useEffect }       from 'react';
import styles                     from './ImageView.module.css';
import { State }                  from '../../store';
import { connect }                from 'react-redux';
import { match }                  from 'react-router-dom';
import { getImages, fetchImages } from '../../store/images';
import { Image as image }         from '../../api/types';

import ImagePanel                 from './components/ImagePanel';
import NavigationBar              from '../../components/NavigationBar';
import SearchSidebar              from '../../components/SearchSideBar';
import { MetaDataPanel }          from '../../components/SearchSideBar/panels';

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

const Image = (props: Props) => {
    useEffect(() => {
        return;
        props.fetchImages()
    }, []);

    const image = props.images.find(image => {
        return image.UUID === props.match.params.uuid;
    });

    return !image
        ? <span>Ruh oh</span>
        : (
        <div className="Page">
            <NavigationBar username={props.username} />

            <div className={styles.Root}>
                <SearchSidebar
                    tags={image.tags}
                    initialPanel="metadata"
                >
                    {{
                       metadata: MetaDataPanel
                    }}
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
