import React, { useEffect } from 'react';
import { State }            from '../../store';
import { connect }          from 'react-redux';
import { fetchImages }      from '../../store/images';
import { image }            from '../../types/image';

import ImageGalleries       from './components/ImageGalleries';
import NavigationBar        from '../../components/NavigationBar';
import SearchSidebar        from '../../components/SearchSideBar';
import { MetaDataPanel }    from '../../components/SearchSideBar/panels';
import styles               from './ImageGalleries.module.css';

const onlyImage: image = {
    createdAt: '01/01/1999',
    hash: 'asd98u23',
    path: 'https://i.imgur.com/aG7C0Zx.jpg',
    resolution: '800x600',
    tags: [
        "cartoon",
        "example",
        "apple"
    ],
    uploader: 'Reisen',
};

interface Props {
    image?: image;
    username: string;
    fetchImages: () => void;
}

const Image = (props: Props) => {
    useEffect(() => { props.fetchImages() }, []);
    return (
        <div className="Page">
            <NavigationBar links={[]} username={props.username} />
            <div className={styles.Root}>
                <SearchSidebar tags={[]} initialPanel="metadata">
                    {{ metadata: MetaDataPanel }}
                </SearchSidebar>
                <ImageGalleries image={onlyImage} />
            </div>
        </div>
    );
}

const mapState = (state: State) => ({
    username: state.user.username
});

const mapDispatch = {
    fetchImages
};


export default connect(mapState, mapDispatch)(Image);
