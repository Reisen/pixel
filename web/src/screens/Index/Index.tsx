// This Screen is the initial gallery index, it shows a paginated view of all
// images currently in the gallery ordered by upload date or tag search. This
// represents the core view for:
//
//   - User's Private Galleries
//   - Public/Shared Galleries

import React, { useEffect } from 'react';
import { Image as image }   from '../../api/types';
import { connect }          from 'react-redux';
import { match }            from 'react-router-dom';
import { State }            from '../../store';
import { ScalingMode }      from '../../store/images/types';
import { History }          from 'history'
import {
    getImages,
    fetchImages,
    setGalleryScaling,
    getGalleryScaling
}                           from '../../store/images';

// Components
import NavigationBar        from '../../components/NavigationBar';
import SearchSideBar        from '../../components/SearchSideBar';
import TagPanel             from '../../panels/TagPanel';
import SettingsPanel        from '../../panels/SettingsPanel';
import ImageGrid            from './components/ImageGrid';
import Pager                from './components/Pager';
import styles               from './Index.module.css';


interface Params {
    page?: string;
}

interface Props {
    fetchImages:        () => void;
    setGalleryScaling:  (mode: ScalingMode) => void;
    galleryScalingMode: ScalingMode;
    images:             image[];
    username:           string;
    history:            History;
    match:              match<Params>;
}

// On each page, the tag list on the left should be a view of the most common
// tags in the _current_ view, I.E, all the tags of all the current visible
// images, ordered by occurance. This function does the ordering and takes the
// top 25 of the slice so the page doesn't grow too much.
const filterVisibleTags = (images: image[]) => {
    let tagCounter: { [index: string]: number } = {};

    // Create Tag List
    images.forEach(image => {
        image.tags.forEach(tag => {
            tagCounter[tag] = tag in tagCounter
                ? tagCounter[tag] + 1
                : 1
        });
    });

    let tagList: [string, number][] = Object.entries(tagCounter);
    return tagList.sort((a, b) => b[1] - a[1]).slice(0, 25);
};

const headerLinks = [
    {name: 'Images', path: ''},
    {name: 'Pools', path: ''},
    {name: 'Tags', path: ''}
];

const panels = (tags: [string, number][]) => {
    return [
        {
            icon: 'tag',
            name: 'Tag List',
            elem: <TagPanel tags={tags} />
        },
        {
            icon: 'gears',
            name: 'Settings',
            elem: <SettingsPanel />
        }
    ];
}

const Index = (props: Props) => {
    // Translate Page number into a slice of the gallery list.
    const pageNumber = parseInt(props.match.params.page || '1', 10);
    const changePage = (page: number) => {
        props.history.push(`/my/images/${page}`)
    };

    // Configure the grid size so we can slice correct amounts of images for
    // each visible page.
    const rows       = 4;
    const cols       = 6;
    const slice      = props.images.slice(
        cols * rows * (pageNumber - 1 + 0),
        cols * rows * (pageNumber - 1 + 1)
    );
    const tags       = filterVisibleTags(slice);

    // Load Images, On First Mount Only, we only re-request on page reload or
    // when a new search is made.
    useEffect(() => { props.fetchImages() }, []);

    return (
        <div className="Page">
            <NavigationBar links={headerLinks} username={props.username} />
            <div className={styles.PanelContainer}>
                <SearchSideBar initialPanel="Tag List">
                    {
                        panels(tags)
                    }
                </SearchSideBar>

                <ImageGrid
                    changePage={changePage}
                    scalingMode={props.galleryScalingMode}
                    setScalingMode={props.setGalleryScaling}
                    images={slice}
                    rows={rows}
                    cols={cols}
                />

                <Pager
                    page={pageNumber}
                    pageCount={5}
                    scalingMode={props.galleryScalingMode}
                    setPage={changePage}
                    setScalingMode={props.setGalleryScaling}
                />
            </div>
        </div>
    );
};

const mapState = (state: State) => ({
    images:             getImages(state.images),
    galleryScalingMode: getGalleryScaling(state.images),
    username:           state.user.username
});

const mapDispatch = {
    fetchImages,
    setGalleryScaling
};

export default connect(mapState, mapDispatch)(Index);
