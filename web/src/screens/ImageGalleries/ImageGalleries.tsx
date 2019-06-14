import React, { useEffect } from 'react';
import { State }            from '../../store';
import { connect }          from 'react-redux';
import { logoutUser }       from '../../store/users';
import { fetchImages }      from '../../store/images';
import { image }            from '../../types/image';

// Components
import ImageGalleries       from './components/ImageGalleries';
import NavigationBar        from '../../components/NavigationBar';
import SearchSidebar        from '../../components/SearchSideBar';
import styles               from './ImageGalleries.module.css';


const onlyImage: image = {
    dimensions: [1600, 1200],
    path: 'https://i.imgur.com/aG7C0Zx.jpg',
    tags: [
        "cartoon",
        "example",
        "apple"
    ],
    uuid: "AAA",
    thumb: "",
    filesize: 0,
    filename: "foo.jpg",
};

interface Props {
    fetchImages: () => void;
    image?:      image;
    logoutUser:  () => void;
    username:    string;
}

const Image = (props: Props) => {
    const { fetchImages } = props
    useEffect(() => { fetchImages() }, [fetchImages]);
    return (
        <div className="Page">
            <NavigationBar
                links={[]}
                username={props.username}
                onLogout={props.logoutUser}
            />

            <div className={styles.Root}>
                <SearchSidebar initialPanel="metadata" />
                <ImageGalleries image={onlyImage} />
            </div>
        </div>
    );
}

const mapState = (state: State) => ({
    username: state.user.username
});

const mapDispatch = {
    fetchImages,
    logoutUser
};


export default connect(mapState, mapDispatch)(Image);
