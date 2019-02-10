import React     from 'react';
import TextInput from '../TextInput'
import styles    from './SearchSideBar.module.css';

interface Props {
    tags?: string[];
}

const SearchSideBar = (props: Props) => (
    <div className={styles.SearchSideBar}>
        <TextInput placeholder="Search" />

        <div className={styles.TagList}>
            {
                !props.tags ? null : props.tags.map(tag => (
                    <a href="#">
                        # { tag }
                    </a>
                ))
            }
        </div>
    </div>
);

export default SearchSideBar;
